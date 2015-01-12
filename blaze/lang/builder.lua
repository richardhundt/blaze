local util = require('blaze.lang.util')
local tree = require('blaze.lang.tree')
local Op   = require('blaze.lang.op')

local Process = require("blaze.lang.process")

local Builder = { } do
   Builder.__index = setmetatable(Builder, tree.Visitor)

   local yield = coroutine.yield

   function Builder.new(ctx)
      return setmetatable({
         ctx = ctx;
      }, Builder)
   end

   function Builder:spawn()
      return Process.new(function(input)
         for chunk in input do
            chunk.optree = self:build(chunk)
            yield(chunk)
         end
      end)
   end

   function Builder:build(chunk)
      self.chunk = chunk
      self.scope = chunk.scope
      return self:get(chunk.tree)
   end

   function Builder:sync(node)
      return self.ctx:sync_line(node.line)
   end

   function Builder:list(nodes, ...)
      local list = { }
      for i=1, #nodes do
         list[#list + 1] = self:get(nodes[i], ...)
      end
      return list
   end

   local B = { }
   function Builder:get(node, ...)
      if not B[node.tag] then
         error("no handler for "..tostring(node.tag))
      end
      self.ctx:sync_line(node.line)
      return B[node.tag](self, node, ...)
   end

   function B:Chunk(node, opts)
      local chunk = { }

      -- import magic from core

      if not self.imports["blaze.core"] then
         local path = package.searchpath("blaze.core", package.path)
         local file = assert(io.open(path))
         local code = file:read("*a")
         file:close()
         code = string.dump(loadstring(code, "@"..path), not DEBUG)

         self.imports["blaze.core"] = path

         chunk[#chunk + 1] =
            Op{'!assign',
               Op{'!index',
                  Op{'!index', 'package', Op.const"preload"},
                  Op.const("blaze.core")
               },
               Op{'!call',
                  'assert',
                  Op{'!call',
                     'loadstring', Op.const(code), Op.const("@"..path)
                  }
               }
            }
      end

      chunk[#chunk + 1] = Op{'!line', Op.const(self.name), 1 }
      if not self.opts.eval then
         chunk[#chunk + 1] = Op.chunk{
            Op{'!define', '__magic__',
               Op{'!index',
                  Op{'!call1',
                     'require', Op.const"blaze.core"
                  },
                  Op.const"__magic__"
               }
            },
            Op{'!call', 'module', Op.const(""),
               Op{'!index', '__magic__', Op.const"environ" }
            }
         }

         --[[
         local sym = { }
         for i = 1, #magic do
            sym[#sym + 1] = magic[i]
         end
         chunk[#chunk + 1] = Op{'!define', Op(sym), Op(sym)}
         --]]
      end

      for i=1, #node.body do
         local line = self:sync(node.body[i])
         local stmt = self:get(node.body[i])
         self:shift(chunk)
         chunk[#chunk + 1] = Op.list{Op{'!line', line}, stmt}
      end

      local seen_export = self.seen_export
      if seen_export then
         self:hoist(Op{'!define', 'export', Op.const{ }})
      end
      self:leave(chunk)

      if seen_export then
         chunk[#chunk + 1] = Op{'!return', 'export'}
      end

      return Op.chunk(chunk)
   end

   function B:ModuleDeclaration(node)
      local path = self:list(node.path)
      local name = table.concat(path, ".")
      if self.modules[name] then
         self.scope = self.modules[name]
      else
         self.modules[name] = self.scope
      end
      return Op{'!call', 'module', Op.const(name),
         Op{'!index', '__magic__', Op.const'environ'}
      }
   end

   local translate

   local function import_macro_func(import, package_name, func_name)
      local errs = string.format(
         "imported macro body for '%s' cannot be resolved",
         name)
      assert(type(package_name) == 'string', errs)
      package_name = util.unquote(package_name)
      local func = import(package_name, func_name)
      assert(func ~= nil, errs)
      return func
   end

   local function iterate_imported_symbols(import_stmt_node)
      local function iterator(names_num, i)
         if i >= names_num then return end
         i = i + 1
         local current_name = import_stmt_node.names[i]
         local imported_symbol_alias = current_name[1].name
         local imported_symbol
         if current_name[2] then
            imported_symbol = current_name[2].name
         else
            imported_symbol = imported_symbol_alias
         end
         return i, imported_symbol_alias, imported_symbol
      end
      return iterator, #import_stmt_node.names, 0
   end

   function B:ImportStatement(node)
      local name = table.concat(self:get(node.from), ".")
      local stmt = Op.chunk{ }

      if not self.imports[name] then
         stmt:push(
            Op{'!assign',
               Op{'!index',
                  Op{'!index', 'package', Op.const"preload"},
                  Op.const(name)
               },
               Op{'!call',
                  'assert',
                  Op{'!call', 'loadstring', Op.const(code), Op.const("@"..name)}
               }
            }
         )
      end

      if #node.names == 0 then
         stmt:push(
            Op{'!call', 'import', Op.const(name), Op.const"*", '_M' }
         )
      else
         local args = { self:get(node.from) }
         local syms = { }
         for i=1, #node.names do
            local n = node.names[i]
            if n[2] then -- got alias
               args[#args + 1] = Op.const(n[2].name)
            else
               args[#args + 1] = Op.const(n[1].name)
            end
            syms[#syms + 1] = n[1].name
         end

         if self.opts.eval then
            stmt:push(
               Op{'!massign',
                  Op(syms),
                  Op{ Op{'!call', 'import', unpack(args) } }
               }
            )
         else
            stmt:push(
               Op{'!define',
                  Op(syms),
                  Op{ Op{'!call', 'import', unpack(args) } }
               }
            )
         end
      end
      return stmt
   end

   function B:ExportStatement(node)
      local ops = { }
      self.seen_export = true
      for i=1, #node.names do
         local expr = Op{'!index', 'export', Op.const(self:get(node.names[i])) }
         ops[#ops + 1] = Op{'!assign', expr, self:get(node.names[i]) }
      end
      return Op.chunk(ops)
   end

   function B:Literal(node)
      return Op.const(node.value)
   end

   function B:Identifier(node)
      if node.name == '__FILE__' then
         return Op.const(self.name)
      end
      if node.name == '__LINE__' then
         return Op.const(self.line)
      end
      if node.check then
         local info = self:lookup(node.name)
         if info == nil then
            self:resolve(node.name)
         end
      end
      return node.name
   end

   function B:MacroDeclaration(node)
      local eval = self.opts.eval
      self.opts.eval = true

      local name = node.name.name
      local core = require("core")
      local func

      if node.head == '=' then
         local nref = node.body.name
         local info = self:lookup(nref)
         if info.kind == 'import' then
            local from = self:get(info.node.from)
            local function import(package_name, func_name)
               return require(package_name)[func_name]
            end
            for i, alias, func_name in iterate_imported_symbols(info.node) do
               if alias == nref then
                  func = import_macro_func(import, from, func_name)
                  break
               end
            end
         elseif info.kind == 'function' then
            local defn = self:get(info.node)
            local wrap = Op.chunk{ defn, Op{'!return', nref} }
            wrap = assert(tvm.load(tostring(wrap)))
            setfenv(wrap, core.__magic__.environ({ }))
            func = wrap()
         end
      else
         local head = self:list(node.head)
         local body = self:list(node.body)

         local wrap = Op.chunk{
            Op{'!return', Op{'!lambda', Op{ Op.list(head) }, Op.chunk(body) } }
         }
         wrap = assert(tvm.load(tostring(wrap)))
         setfenv(wrap, core.__magic__.environ({ }))
         func = wrap()
      end

      self.opts.eval = eval
      self.scope.macro[name] = func

      return Op.chunk{ }
   end

   local function extract_bindings(node, ident)
      local list = { }
      local queue = { node }
      while #queue > 0 do
         local n = table.remove(queue)
         if n.tag == 'ArrayPattern' then
            for i=#n.elements, 1, -1 do
               queue[#queue + 1] = n.elements[i]
            end
         elseif n.tag == 'TablePattern' then
            for i=#n.entries, 1, -1 do
               queue[#queue + 1] = n.entries[i].value
            end
         elseif n.tag == 'ApplyPattern' then
            for i=#n.arguments, 1, -1 do
               queue[#queue + 1] = n.arguments[i]
            end
         elseif n.tag == 'Identifier' then
            list[#list + 1] = n
         elseif n.tag == 'MemberExpression' then
            if ident then
               queue[#queue + 1] = n.object
            else
               list[#list + 1] = n
            end
         else
            assert(n.tag == 'Literal')
         end
      end
      return list
   end

   function B:LocalDeclaration(node)
      node.is_local = true

      local decl = { }
      for i=1, #node.names do
         local list = extract_bindings(node.names[i])
         for i=1, #list do
            if list[i].tag == 'Identifier' then
               decl[#decl + 1] = list[i].name
            end
         end
      end

      node.left  = node.names
      node.right = node.inits
      local frag = Op.chunk{ B.AssignmentExpression(self, node) }

      for i=#node.decorators, 1, -1 do
         local deco = node.decorators[i]
         frag[#frag + 1] = Op{'!massign', Op(decl),
            Op{Op{'!call', self:get(deco.term), unpack(decl) }}
         }
      end

      return frag
   end

   function B:AssignmentExpression(node)
      local body = { }
      local decl = { }
      local init = { }
      local dest = { }
      local chks = { }

      local exps
      if node.right then
         exps = self:list(node.right)
      else
         exps = Op{Op.const(nil)}
      end

      for i=1, #node.left do
         local n = node.left[i]
         local t = n.tag
         if t == 'TablePattern' or t == 'ArrayPattern' or t == 'ApplyPattern' then
            -- destructuring
            local temp = util.genid()

            local left = { }
            n.temp = temp
            n.left = left

            init[#init + 1] = temp
            decl[#decl + 1] = temp
            dest[#dest + 1] = n

            -- define new variables
            local bind = extract_bindings(n)
            for i=1, #bind do
               local n = bind[i]
               if n.tag == 'Identifier' then
                  if node.is_local or n.guard or not self:lookup(n.name) then
                     local guard
                     if n.guard then
                        guard = util.genid()
                        body[#body + 1] = Op{'!let', guard, self:get(n.guard)}
                     end
                     self:define(n.name, nil, guard)
                     if not self.opts.eval then
                        decl[#decl + 1] = n.name
                     end
                  end
                  if self:lookup(n.name).guard then
                     chks[#chks + 1] = self:lookup(n.name)
                  end
                  left[#left + 1] = n.name
               elseif n.tag == 'MemberExpression' then
                  left[#left + 1] = self:get(n)
               end
            end
         else
            -- simple case
            if n.tag == 'Identifier' then
               if node.is_local or n.guard or not self:lookup(n.name) then
                  local guard
                  if n.guard then
                     guard = util.genid()
                     body[#body + 1] = Op{'!let', guard, self:get(n.guard)}
                  end
                  self:define(n.name, nil, guard)
                  if not self.opts.eval then
                     decl[#decl + 1] = n.name
                  end
               end
               if self:lookup(n.name).guard then
                  chks[#chks + 1] = self:lookup(n.name)
               end
            end
            init[#init + 1] = self:get(n)
         end
      end

      -- declare locals
      if #decl > 0 then
         if #decl == 0 then
            body[#body + 1] = Op{'!define', Op(decl)}
         else
            body[#body + 1] = Op{'!define', Op(decl), Op{Op.const(nil)}}
         end
      end

      for i=1, #dest do
         local patt = util.genid()
         body[#body + 1] = Op{'!define', Op{ patt }, Op{ self:get(dest[i])} }
         dest[i].patt = patt
      end

      body[#body + 1] = Op{'!massign', Op(init), Op(exps) }

      -- destructure
      for i=1, #dest do
         body[#body + 1] = Op{'!massign',
            Op(dest[i].left),
            Op{ Op{'!call', '__extract__', dest[i].patt, dest[i].temp } } }
      end

      for i=1, #chks do
         body[#body + 1] = Op{'!call', '__check__',
            Op.const(chks[i].name), chks[i].name, chks[i].guard }
      end

      return Op.chunk(body)
   end

   function B:ArrayPattern(node)
      local list = { }
      for i=1, #node.elements do
         local n = node.elements[i]
         if n.tag == 'Identifier' or n.tag == 'MemberExpression' then
            list[#list + 1] = '__var__'
         else
            list[#list + 1] = self:get(n)
         end
      end
      return Op{'!call', 'ArrayPattern', unpack(list) }
   end

   function B:TablePattern(node)
      local idx = 1
      local keys = { }
      local desc = { }
      for i=1, #node.entries do
         local n = node.entries[i]

         local key, val
         if n.name then
            key = Op.const(n.name.name)
         elseif n.expr then
            key = self:get(n.expr)
         else
            -- array part
            key = Op(idx)
            idx = idx + 1
         end
         local nv = n.value
         if nv.tag == 'Identifier' or nv.tag == 'MemberExpression' then
            val = '__var__'
         else
            val = self:get(nv)
         end
         keys[#keys + 1] = key
         desc[key] = val
      end
      keys = Op.const(keys)
      desc = Op.const(desc)
      local args = { keys, desc }
      if node.coerce then
         args[#args + 1] = self:get(node.coerce)
      end
      return Op{'!call', 'TablePattern', unpack(args)}
   end

   function B:ApplyPattern(node)
      local args = { self:get(node.callee) }
      for i=1, #node.arguments do
         local n = node.arguments[i]
         if n.tag == 'Identifier' or n.tag == 'MemberExpression' then
            args[#args + 1] = '__var__'
         else
            args[#args + 1] = self:get(n)
         end
      end
      return Op{'!call', 'ApplyPattern', unpack(args)}
   end

   function B:InExpression(node)
      local names = { }
      for i=1, #node.names do
         names[#names + 1] = Op.const(node.names[i].name)
      end
      return Op{ '!call', '__in__', Op.const(names), self:get(node.expression) }
   end

   function B:UpdateExpression(node)
      local oper = string.sub(node.operator, 1, -2)
      local expr
      if oper == 'or' or oper == 'and' then
         expr = B.LogicalExpression(self, {
            operator = oper,
            left     = node.left,
            right    = node.right
         })
      else
         expr = B.BinaryExpression(self, {
            operator = oper,
            left     = node.left,
            right    = node.right
         })
      end
      return Op{'!assign', self:get(node.left), expr}
   end

   function B:MemberExpression(node)
      if node.computed then
         return Op{'!index', self:get(node.object), self:get(node.property)}
      else
         return Op{'!index', self:get(node.object), Op.const(self:get(node.property))}
      end
   end

   function B:SelfExpression(node)
      return 'self'
   end

   function B:SuperExpression(node)
      return 'super'
   end

   function B:ThrowStatement(node)
      return Op{'!call', 'throw', self:get(node.argument)}
   end

   function B:TakeStatement(node)
      local args = self:list(node.arguments)
      return Op{'!call', '__take__', unpack(args)}
   end

   function B:ReturnStatement(node)
      if self.scope.stash['is_expr'] then
         self:abort("illegal return in expression")
      end
      local args = self:list(node.arguments)
      if self.retsig then
         return Op{'!do',
            Op{'!assign', self.retsig, '!true' },
            Op{'!assign', self.retval, #args > 0 and args[1] or '!nil'},
            Op{'!return', self.retval },
         }
      end
      return Op{'!return', unpack(args)}
   end

   function B:IfStatement(node)
      local test, cons, altn = self:get(node.test), nil, nil
      if node.consequent then
         self:enter()
         cons = self:get(node.consequent)
         self:leave()
      end
      if node.alternate then
         self:enter()
         altn = self:get(node.alternate)
         self:leave()
      end
      return Op{'!if', test, Op{'!do', cons}, Op{'!do', altn } }
   end

   function B:GivenStatement(node)
      local body = { }
      local disc = util.genid()

      body[#body + 1] = Op{'!define', disc, self:get(node.discriminant) }

      local labels = { }

      for i=1, #node.cases do
         labels[#labels + 1] = util.genid()
      end

      self:enter()

      for i=1, #node.cases do
         local n = node.cases[i]
         if n.test then
            local t = n.test.tag
            local case = { }
            if t == 'ArrayPattern' or t == 'TablePattern' or t == 'ApplyPattern' then
               local cons = { }

               -- for storing the template
               local temp = util.genid()
               self:define(temp)

               case[#case + 1] = Op{'!define', temp, self:get(n.test) }

               cons[#cons + 1] = Op{'!if',
                  Op{'!not', Op{'!call', '__match__', temp, disc } },
                  Op{'!goto', labels[i] }
               }

               self:enter() -- consequent

               local into = { }
               local bind = extract_bindings(n.test)
               local vars = { }
               local chks = { }
               for i=1, #bind do
                  local n = bind[i]
                  if n.tag == 'Identifier' then
                     local guard
                     if n.guard then
                        guard = util.genid()
                        case[#case + 1] = Op{'!let', guard, self:get(n.guard)}
                     end
                     self:define(n.name, nil, guard)
                     if guard then
                        chks[#chks + 1] = self:lookup(n.name)
                     end
                     vars[#vars + 1] = n.name
                  end
                  bind[i] = self:get(n)
               end

               if #vars > 0 then
                  case[#case + 1] = Op{'!define', Op(vars), Op{ Op.const(nil) } }
               end

               cons[#cons + 1] = Op{'!massign',
                  Op(bind), Op{ Op{'!call', '__extract__', temp, disc } }
               }

               for i=1, #chks do
                  cons[#cons + 1] = Op{'!call', '__check__',
                     Op(chks[i].name), chks[i].name, chks[i].guard }
               end

               if n.guard then
                  cons[#cons + 1] = Op{'!if',
                     Op{'!not', self:get(n.guard) }, Op{'!goto', labels[i] }
                  }
               end

               cons[#cons + 1] = self:get(n.consequent)
               self:leave()

               case[#case + 1] = Op{'!do', Op.chunk(cons)}
               case[#case + 1] = Op{'!goto', labels[#labels] }
            else
               case[#case + 1] = Op{'!if',
                  Op{'!not', Op{'!call', '__match__', self:get(n.test), disc } },
                  Op{'!goto', labels[i] }
               }
               if n.guard then
                  case[#case + 1] = Op{'!if',
                     Op{'!not', self:get(n.guard) }, Op{'!goto', labels[i] }
                  }
               end
               case[#case + 1] = self:get(n.consequent)
               case[#case + 1] = Op{'!goto', labels[#labels] }
            end
            body[#body + 1] = Op{'!do', Op.chunk(case) }
         else
            -- else clause
            body[#body + 1] = Op{'!do', self:get(n.consequent) }
         end
         body[#body + 1] = Op{'!label', labels[i]}
      end

      self:leave(body)

      return Op{'!do', Op.chunk(body) }
   end

   function B:TryStatement(node)
      local oldret = self.retsig
      local oldval = self.retval
      local oldbrk = self.brksig
      local oldcnt = self.cntsig

      self.retsig = util.genid()
      self.retval = util.genid()
      self.brksig = util.genid()
      self.cntsig = util.genid()

      local try = Op{'!lambda', Op{ }, self:get(node.body)}

      local finally
      if node.finalizer then
         finally = Op{'!lambda', Op{ }, self:get(node.finalizer)}
      end

      local exit = util.genid()

      local clauses = { }
      for i=#node.guardedHandlers, 1, -1 do
         local clause = node.guardedHandlers[i]
         self:define(clause.param.name)
         local cons = self:get(clause.body)
         local head = Op{'!define', self:get(clause.param), '!vararg'}
         cons[#cons + 1] = Op{'!goto', exit }
         clauses[#clauses + 1] = Op{'!do', head,
            Op{'!if', self:get(clause.guard), Op{'!do', Op.chunk(cons)}} }
      end
      if node.handler then
         local clause = node.handler
         self:define(clause.param.name)
         local cons = self:get(clause.body)
         local head = Op{'!define', self:get(clause.param), '!vararg'}
         cons[#cons + 1] = Op{'!goto', exit}
         clauses[#clauses + 1] = Op{'!do', head, Op{'!do', Op.chunk(cons)}}
      end
      clauses[#clauses + 1] = Op{'!label', exit }

      local catch = Op{'!lambda', Op{'!vararg'}, Op.chunk(clauses)}

      local expr = Op{'!call', 'try', try, catch, finally }

      local temp = self.retval
      local rets = self.retsig
      local brks = self.brksig
      local cnts = self.cntsig

      self.retsig = oldret
      self.retval = oldval
      self.brksig = oldbrk
      self.cntsig = oldcnt

      local frag = Op{'!do',
         Op{'!define', Op{ rets, brks, cnts }, Op{ '!false', '!false', '!false' } },
         Op{'!define', temp, Op(nil) },
         Op(expr),
         Op{'!if', rets, Op{'!return', temp } }
      }
      if self.loop then
         frag[#frag + 1] = Op{'!if', cnts, Op{'!goto', self.loop} }
         frag[#frag + 1] = Op{'!if', brks, Op{'!break'} }
      end
      return frag
   end

   function B:LabelStatement(node)
      return Op{'!label', node.label.name }
   end
   function B:GotoStatement(node)
      return Op{'!goto', node.label.name }
   end

   function B:BreakStatement(node)
      if self.brksig then
         return Op.chunk{
            Op{'!assign', self.brksig, '!true'},
            Op{'!return'}
         }
      end
      return Op{'!break'}
   end

   function B:ContinueStatement(node)
      if not self.loop then
         self:fail("no loop to continue")
      end
      if self.cntsig then
         return Op.chunk{
            Op{'!assign', self.cntsig, '!true'},
            Op{'!return'}
         }
      end
      return Op{'!goto', self.loop}
   end

   function B:LogicalExpression(node)
      local op = node.operator
      if op == 'and' then
         return Op{'!and', self:get(node.left), self:get(node.right) }
      elseif op == 'or' then
         return Op{'!or', self:get(node.left), self:get(node.right) }
      else
         assert(false, "Unhandled operator "..op.." in logical expression")
      end
   end

   local bitop = {
      [">>"]  = '__rshift__',
      [">>>"] = '__arshift__',
      ["<<"]  = '__lshift__',
      ["|"]   = '__bor__',
      ["&"]   = '__band__',
      ["^"]   = '__bxor__',
   }
   local binop = {
      ['+']  = '!add',
      ['-']  = '!sub',
      ['*']  = '!mul',
      ['/']  = '!div',
      ['%']  = '!mod',
      ['**'] = '!pow',
      ['~']  = '!concat',
      ['=='] = '!eq',
      ['!='] = '!ne',
      ['>='] = '!ge',
      ['<='] = '!le',
      ['>']  = '!gt',
      ['<']  = '!lt',
   }
   function B:BinaryExpression(node)
      local o = node.operator
      if bitop[o] then
         return Op{'!call', bitop[o], self:get(node.left), self:get(node.right) }
      end
      if o == 'is' then
         return Op{'!call', '__is__', self:get(node.left), self:get(node.right)}
      end
      if o == 'as' then
         return Op{'!call', '__as__', self:get(node.left), self:get(node.right)}
      end
      if o == "with" then
         return Op{'!call', '__with__', self:get(node.left), self:get(node.right)}
      end
      if o == '..' then
         return Op{'!call', '__range__', self:get(node.left), self:get(node.right)}
      end
      if o == "~~" then
         return Op{'!call', '__match__', self:get(node.right), self:get(node.left)}
      end
      if o == "!~" then
         return Op{'!not', Op{'!call', '__match__', self:get(node.right), self:get(node.left)} }
      end
      if string.sub(o, 1, 1) == ':' then
         return Op{'!call', '__usrop__', Op(o), self:get(node.left), self:get(node.right) }
      end
      return Op{binop[o], self:get(node.left), self:get(node.right)}
   end

   local unop = {
      ['#']   = '!len',
      ['-']   = '!neg',
      ['!']   = '!not',
      ['not'] = '!not',
   }
   function B:UnaryExpression(node)
      local o = node.operator
      local a = self:get(node.argument)
      if o == '~' then
         return Op{'!call', '__bnot__', a }
      end
      return Op{unop[o], a }
   end

   function B:ParenExpression(node)
      return self:get(node.expression)
   end

   local function apply_decorators(self, node, decl)
      if node.decorators ~= nil and #node.decorators > 0 then
         for i=#node.decorators, 1, -1 do
            local deco = node.decorators[i]
            decl = Op{'!call1', self:get(deco.term), decl }
         end
      end
      return decl
   end

   function B:FunctionDeclaration(node)
      local name
      if not node.expression then
         name = self:get(node.name)
         if node.name.tag == 'Identifier' then
            if node.islocal or self:in_environ() then
               self:define(name, { tag = "function", node = node })
            else
               -- in function scope, hoist it
               self:define(name, {
                  kind = "function",
                  line = self.scope.topline,
                  node = node
               })
               self:hoist(Op{'!define', name })
            end
         end
      end

      local params  = { }
      local prelude = { }

      self:enter("function")

      for i=1, #node.params do
         self:define(node.params[i].name)
         local name = self:get(node.params[i])
         params[#params + 1] = name
         if node.defaults[i] then
            local test = Op{'!eq', name, '!nil'}
            local expr = self:get(node.defaults[i])
            local cons = Op{'!assign', name, expr }
            prelude[#prelude + 1] = Op{'!if', test, cons }
         end
         if node.guards[i] then
            local expr = self:get(node.guards[i])

            -- hoist guards constructors to the outer scope
            local temp = util.genid()
            self:push(Op{'!let', temp, expr})
            expr = temp

            local test = Op{'!call', '__is__', name, expr }
            local mesg
            if i == 1 and expr == '__self__' then
               mesg = "calling '%s' on bad self (%s expected got %s)"
            else
               mesg = string.format(
                  "bad argument #%s to '%%s' (%%s expected got %%s)", i
               )
            end
            local level = node.level or 1
            local cons
            if node.name then
               cons = Op{'!call', 'error',
                  Op{'!callmeth', Op.const(mesg), 'format',
                     Op.const(self:get(node.name)),
                     Op{'!call1', 'tostring', expr },
                     Op{'!call1', 'typeof', name }
                  }, Op.const(level + 1)
               }
            else
               cons = Op{'!call', 'error',
                  Op{'!callmeth', Op.const(mesg), 'format',
                     Op{'!or',
                        Op{'!index',
                           Op{'!call1',
                              Op{'!index', 'debug', Op.const"getinfo"},
                              Op.const(level), Op.const"n" },
                           Op.const"name"
                        },
                        Op.const('?')
                     },
                     Op{'!call1', 'tostring', expr },
                     Op{'!call1', 'typeof', name }
                  }, Op.const(level + 1)
               }
            end
            prelude[#prelude + 1] = Op{'!if', Op{'!not', test }, cons }
         end
      end

      if node.rest then
         params[#params + 1] = '!vararg'
         if node.rest ~= "" then
            self:define(node.rest.name)
            prelude[#prelude + 1] = Op{'!define', node.rest.name,
               Op{'!call1', 'Array', '!vararg' } }
         end
      end

      local body = self:get(node.body)

      for i=#prelude, 1, -1 do
         table.insert(body, 1, prelude[i])
      end

      self:leave(body)

      local func
      if node.generator then
         local inner = Op{'!lambda', Op{ }, body}
         func = Op{'!lambda', Op(params),
            Op.chunk{
               Op{'!return', Op{'!call1', Op{'!index', 'coroutine', Op"wrap"}, inner}}
            }}
      else
         func = Op{'!lambda', params, unpack(body)}
      end

      if node.expression then
         return func
      end

      func = apply_decorators(self, node, func)

      local decl
      if node.islocal then
         decl = Op.chunk{
            Op{'!define', name},
            Op{'!assign', name, func}
         }
      else
         decl = Op{'!assign', name, func }
      end

      local wrap = Op.chunk{ }
      self:shift(wrap)
      wrap[#wrap + 1] = decl
      return wrap
   end

   function B:IncludeStatement(node)
      local path = self:get(node.part)[1]
      local file = io.open(path)
      local ssrc = file:read("*a")
      file:close()
      local tree = parser.parse(ssrc, path, 1, self.opts)
      --chunk[#chunk + 1] = Op{'!line', Op.const(self.name), 1 }

      local line = self:sync(node)
      local list = self:list(tree.body)
      table.insert(list, 1, Op{'!line', Op.const(path), 1 })
      self.line = line
      table.insert(list, Op{'!line', Op.const(self.name), line })
      return Op.chunk(list)
      --[[
      return Op.chunk {
         Op{'!assign',
            Op{'!index',
               Op{'!index', 'package', Op.const"preload"},
               Op.const(name)
            },
            Op{'!call',
               'assert',
               Op{'!call', 'loadstring', Op.const(code), Op.const("="..path)}
            }
         }
      }
      --]]
   end

   function B:TraitDeclaration(node)
      local name = self:get(node.id)
      if self:in_environ() and node.scope ~= 'local' then
         self:define(name)
      else
         self:define(name, { line = self.scope.topline })
         self:hoist(Op{'!define', name})
      end

      self:enter("environ")
      self:define('self')
      self:define('__self__')

      self:hoist(Op{'!let', Op{node.id.name}, Op{'self'}})

      local body = self:get(node.body)

      self:unhoist(body)
      self:leave()

      local init = Op{'!call', 'trait', Op.const(node.id.name),
         Op{'!lambda', Op{ 'self', '!vararg' }, body } }

      init = apply_decorators(self, node, init)

      return Op{'!assign', name, init}
   end

   function B:ClassDeclaration(node)
      local name = self:get(node.id)
      if self:in_environ() and node.scope ~= 'local' then
         self:define(name)
      else
         self:define(name, { line = self.scope.topline })
         self:hoist(Op{'!define', name})
      end

      local base = node.base and self:get(node.base) or nil
      self:enter("environ")

      self:define('self')
      self:define('super')
      self:define('__self__')

      self:hoist(Op{'!let', { node.id.name }, { 'self' } })

      local body = self:get(node.body)

      self:unhoist(body)
      self:leave()

      local init = Op{'!call', 'class', Op.const(node.id.name),
         Op{'!lambda', Op{ 'self', 'super' }, body }, base }

      init = apply_decorators(self, node, init)

      return Op{'!assign', name, init}
   end

   function B:ClassBodyStatement(node, body)
      body = body or Op.chunk{ }
      local line = self:sync(node)
      line = Op{'!line', line }
      if node.tag == "PropertyDefinition" then
         local prop = node
         if prop.kind == "get" then
            -- self.__getters__[key] = desc.get
            prop.value.name = prop.key
            prop.value.level = 2

            local decl = self:get(prop)
            self:shift(body)
            decl = apply_decorators(self, prop, decl)

            body[#body + 1] = Op.list{line, Op{'!assign',
               Op{'!index',
                  Op{'!index', 'self', Op.const"__getters__" },
               Op.const(prop.key.name) }, decl }}

         elseif prop.kind == "set" then
            -- self.__setters__[key] = desc.set
            prop.value.name = prop.key
            prop.value.level = 2

            local decl = self:get(prop)
            self:shift(body)
            decl = apply_decorators(self, prop, decl)

            body[#body + 1] = Op.list{line, Op{'!assign',
               Op{'!index',
                  Op{'!index', 'self', Op.const"__setters__" },
               Op.const(prop.key.name) }, decl }}
         else
            -- hack to skip a frame for the constructor
            if prop.key.name == 'self' then
               prop.value.level = 2
            end

            local decl = self:get(prop)
            self:shift(body)
            decl = apply_decorators(self, prop, decl)

            -- self.__members__[key] = desc.value
            body[#body + 1] = Op.list{line, Op{'!assign',
               Op{'!index',
                  Op{'!index', 'self', Op.const"__members__" },
               Op.const(prop.key.name) }, decl }}
         end
      elseif node.tag == 'ClassDeclaration'
          or node.tag == 'TraitDeclaration'
          or node.tag == 'GrammarDeclaraion'
      then

         local stmt = self:get(node)
         self:shift(body)
         body[#body + 1] = stmt

         if node.scope ~= 'local' then
            local inner_name = self:get(node.id)
            body[#body + 1] = Op.list{line,
               Op{'!assign', Op{'!index', 'self', Op.const(inner_name)}, inner_name }
            }
         end
      else
         local stmt = self:get(node)
         self:shift(body)
         body[#body + 1] = Op.list{line, stmt}
      end
   end

   function B:SpreadExpression(node)
      if node.argument ~= '...' then
         return Op{'!call', '__spread__', self:get(node.argument) }
      else
         return '!vararg'
      end
   end

   function B:NilExpression(node)
      return '!nil'
   end

   function B:PropertyDefinition(node)
      node.value.generator = node.generator
      return self:get(node.value)
   end

   function B:DoStatement(node)
      return Op{'!do', self:get(node.body)}
   end

   function B:Block(node)
      local body = Op.chunk{ }
      local scope = self.scope
      for i=1, #node do
         local n = node[i]
         local line = self:sync(n)
         if i == #node then
            if scope.kind == "function" then
               scope.stash["is_last"] = true
            end
         end

         local stmt = self:get(n)
         if i == #node and n.tag == 'ExpressionStatement' and
            n.expression.tag ~= 'AssignmentExpression' and
            n.expression.tag ~= 'UpdateExpression' and
            scope.stash["is_last"] and not scope.stash['is_loop']
         then
            body[#body + 1] = Op.list{Op{'!line', line}, Op{'!return', stmt}}
         else
            body[#body + 1] = Op.list{Op{'!line', line}, stmt}
         end
         self:shift(body)
      end
      return body
   end

   function B:ExpressionStatement(node)
      if node.expression.tag == 'Identifier' then
         return Op{'!call', self:get(node.expression)}
      end
      local line = self:sync(node)
      local scope = self.scope
      local save = scope.stash['is_expr']
      scope.stash['is_expr'] = true
      local expr = self:get(node.expression)
      scope.stash['is_expr'] = save
      return Op.list{Op{'!line', line}, expr}
   end

   function B:CallExpression(node)
      local callee = node.callee
      if callee.tag == 'MemberExpression' and not callee.computed then
         if callee.object.tag == 'SuperExpression' then
            local args = self:list(node.arguments)
            local recv = Op{'!index', 'super', Op.const(self:get(callee.property)) }
            table.insert(args, 1, 'self')
            return Op{'!call', recv, unpack(args)}
         else
            if callee.namespace then
               return Op{'!call', self:get(callee), unpack(self:list(node.arguments))}
            else
               local recv = self:get(callee.object)
               local prop = self:get(callee.property)
               return Op{'!callmeth', recv, prop, unpack(self:list(node.arguments))}
            end
         end
      else
         if callee.tag == 'SuperExpression' then
            local args = self:list(node.arguments)
            local recv = Op{'!index', 'super', Op.const("self")}
            table.insert(args, 1, 'self')
            return Op{'!call', recv, unpack(args)}
         else
            local scope = self.scope
            if callee.tag == 'Identifier' and scope.macro[callee.name] then
               local macro = scope.macro[callee.name]
               local frag  = macro(self, unpack(node.arguments))
               return frag
            else
               local args = self:list(node.arguments)
               return Op{'!call', self:get(callee), unpack(args)}
            end
         end
      end
   end

   function B:WhileStatement(node)
      local loop = util.genid()
      local save = self.loop
      self.loop = loop
      self:enter()
      self.stash['is_loop'] = true
      local body = self:get(node.body)
      body[#body + 1] = Op{'!label', loop}
      self:leave()
      self.loop = save
      return Op{'!while', self:get(node.test), body}
   end

   function B:RepeatStatement(node)
      local loop = util.genid()
      local save = self.loop
      self.loop = loop
      self:enter()
      self.stash['is_loop'] = true
      local body = self:get(node.body)
      body[#body + 1] = Op{'!label',loop}
      self:leave()
      self.loop = save
      return Op{'!repeat', body, self:get(node.test) }
   end

   function B:ForStatement(node)
      local loop = util.genid()
      local save = self.loop
      self.loop = loop
      self:enter()
      self.stash['is_loop'] = true
      self:define(node.name.name)
      local name = self:get(node.name)
      local init = self:get(node.init)
      local last = self:get(node.last)
      local step = self:get(node.step)
      local body = self:get(node.body)
      body[#body + 1] = Op{'!label',loop}
      self.loop = save
      self:leave()
      return Op{'!loop', name, init, last, step, unpack(body)}
   end

   function B:ForInStatement(node)
      local loop = util.genid()
      local save = self.loop
      self.loop = loop

      local none = util.genid()
      local temp = util.genid()
      local iter = Op{'!call', '__each__', self:get(node.right) }

      self:enter()
      self.stash['is_loop'] = true
      local left = { }
      for i=1, #node.left do
         self:define(node.left[i].name)
         left[i] = self:get(node.left[i])
      end

      local body = self:get(node.body)
      body[#body + 1] = Op{'!label', loop}

      self.loop = save
      self:leave()
      return Op{'!for', Op(left), Op{iter}, unpack(body) }
   end

   function B:RangeExpression(node)
      return Op{'!call1', '__range__', self:get(node.left), self:get(node.left) }
   end

   function B:ArrayExpression(node)
      return Op{'!call1', 'Array', unpack(self:list(node.elements))}
   end

   function B:TableExpression(node)
      local tab = { }
      for i=1, #node.entries do
         local item = node.entries[i]

         local key, val
         if item.name then
            key = Op.const(item.name.name)
         elseif item.expr then
            key = self:get(item.expr)
         end

         local line = Op{'!line', self:sync(item.value) }
         if key ~= nil then
            tab[key] = self:get(item.value)
         else
            tab[#tab + 1] = self:get(item.value)
         end
      end

      return Op.const(tab)
   end

   function B:RawString(node)
      if #node.expressions == 0 then
         return Op.const("")
      elseif #node.expressions == 1 then
         return Op.const(node.expressions[1])
      end
      local list = { }
      for i=1, #node.expressions do
         local expr = node.expressions[i]
         if type(expr) == 'string' then
            list[#list + 1] = Op.const(expr)
         else
            list[#list + 1] = Op{'!call', 'tostring', self:get(expr.expression) }
         end
      end
      return Op{'!mconcat', unpack(list)}
   end

   function B:ArrayComprehension(node)
      local temp = util.genid()
      for i=1, #node.blocks do
         local n = node.blocks[i]
         for j=1, #n.left do
            self:define(n.left[j].name)
         end
      end
      local head = Op{'!define', temp, Op{'!call', 'Array'} };
      local body = Op.chunk{
         Op{'!assign',
            Op{'!index', temp, Op{'!len', temp}},
            self:get(node.body) };
      }
      local tail = Op{'!return', temp }
      for i=1, #node.blocks do
         body = self:get(node.blocks[i], body)
      end
      return Op{'!call', Op{'!lambda', Op{ }, head, body, tail}}
   end

   function B:ComprehensionBlock(node, body)
      local iter = Op{'!call', '__each__', self:get(node.right) }
      local left = self:list(node.left)
      if node.filter then
         body = Op{'!if', self:get(node.filter), Op{'!do', body}}
      end
      return Op{'!for', Op(left), Op{ iter }, body}
   end

   function B:RegExp(node)
      return Op{'!call1',
         Op{'!index', '__rule__', Op.const'P' }, self:get(node.pattern)
      }
   end

   function B:GrammarDeclaration(node)
      local name = self:get(node.id)
      if self:in_environ() and node.scope ~= 'local' then
         self:define(name)
      else
         self:define(name, { line = self.scope.topline })
         self:hoist(Op{'!define', name})
      end

      local base = node.base and self:get(node.base) or nil

      self:enter("environ")
      self:define('self')
      self:define('__self__')

      self:hoist(Op{'!let', Op{node.id.name}, Op{'self'}})

      local body = Op.chunk{ }
      local init = nil
      for i=1, #node.body do
         local n = node.body[i]
         if n.tag == 'PatternRule' then
            if not init then
               init = n.name
               body[#body + 1] = Op{'!assign',
                  Op{'!index', Op{'!index', 'self', Op.const"__members__" }, Op.const(1) },
                  Op.const(n.name)
               }
            end
            self:shift(body)
            body[#body + 1] = Op{'!assign',
               Op{'!index', Op{'!index', 'self', Op.const"__members__"}, Op.const(n.name) },
               self:get(n.body)
            }
         else
            B.ClassBodyStatement(self, n, body)
         end
      end
      if not init then
         self:abort("no initial rule in grammar '"..name.."'")
      end

      self:unhoist(body)
      self:leave()

      body = Op{'!lambda', Op{ 'self', 'super' }, unpack(body) }

      local init = Op{'!call1', 'grammar', Op.const(name), body, base}
      init = apply_decorators(self, node, init)

      return Op{'!assign', name, init }
   end

   function B:PatternGrammar(node)
      local tab = { [1] = Op.const(node.rules[1].name) }
      for i=1, #node.rules do
         local n = node.rules[i]
         local key = Op.const(n.name)
         local val = self:get(n.body)
         tab[key] = Op.list{ Op{'!line', self:sync(n.body) }, val }
      end
      return Op{'!call1', Op{'!index', '__rule__', Op.const'P'}, Op.const(tab) }
   end

   function B:PatternAlternate(node)
      local left, right
      if node.left then
         left  = self:get(node.left)
         right = self:get(node.right)
      else
         left = self:get(node.right)
      end
      local line = self:sync(node)
      return Op.list{
         Op{'!line', line},
         Op{'!call1', Op{'!index', '__rule__', Op.const"__add"}, left, right}
      }
   end

   function B:PatternSequence(node)
      local left, right
      if node.left then
         left  = self:get(node.left)
         right = self:get(node.right)
      else
         left = self:get(node.right)
      end
      local line = self:sync(node)
      return Op.list{
         Op{'!line', line},
         Op{'!call1', Op{'!index', '__rule__', Op.const"__mul"}, left, right}
      }
   end

   function B:PatternAny(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"P"}, Op.const(1)}
   end

   function B:PatternAssert(node)
      local call
      if node.operator == '&' then
         call = '__len'
      else
         call = '__unm'
      end
      return Op{'!call1', Op{'!index', '__rule__', Op.const(call)}, self:get(node.argument)}
   end

   function B:PatternProduction(node)
      local oper, call = node.operator
      if oper == '~>' then
         call = 'Cf'
      elseif oper == '+>' then
         call = 'Cmt'
      else
         assert(oper == '->')
         call = '__div'
      end
      local left  = self:get(node.left)
      local right = self:get(node.right)

      return Op{'!call1', Op{'!index', '__rule__', Op.const(call)}, left, right}
   end

   function B:PatternRepeat(node)
      local left, right = self:get(node.left), Op.const(node.count)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"__pow"}, left, right}
   end

   function B:PatternCaptBasic(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"C"}, self:get(node.pattern)}
   end

   function B:PatternCaptSubst(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Cs"}, self:get(node.pattern)}
   end

   function B:PatternCaptTable(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Ct"}, self:get(node.pattern)}
   end

   function B:PatternCaptConst(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Cc"}, self:get(node.argument)}
   end

   function B:PatternCaptGroup(node)
      local args = { self:get(node.pattern) }
      if node.name then
         args[#args + 1] = Op.const(node.name)
      end
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Cg"}, unpack(args)}
   end

   function B:PatternCaptBack(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Cb"}, Op.const(node.name)}
   end

   function B:PatternCaptBackRef(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Cbr"}, Op.const(node.name)}
   end

   function B:PatternReference(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"V"}, Op.const(node.name)}
   end

   function B:PatternClass(node)
      local expr = self:get(node.alternates)
      if node.negated then
         local any = Op{'!call1', Op{'!index', '__rule__', Op.const"P"}, Op.const(1)}
         expr = Op{'!call1', Op{'!index', '__rule__', Op.const"__sub"}, any, expr}
      end
      return expr
   end

   function B:PatternRange(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"R"}, Op.const(node.left..node.right)}
   end

   function B:PatternTerm(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"P"}, Op.const(node.literal)}
   end

   function B:PatternPredef(node)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Def"}, Op.const(node.name)}
   end

   function B:PatternArgument(node)
      local argn = tonumber(node.index)
      return Op{'!call1', Op{'!index', '__rule__', Op.const"Carg"}, Op.const(argn)}
   end

end

return Builder
