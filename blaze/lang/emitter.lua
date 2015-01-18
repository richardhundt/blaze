local util = require('blaze.lang.util')
local tree = require('blaze.lang.tree')

local Process = require("blaze.lang.process")

local Emitter = { } do
   Emitter.__index = setmetatable(Emitter, tree.Visitor)

   local yield = coroutine.yield

   function Emitter.new(ctx)
      return setmetatable({
         ctx     = ctx;
         imports = { };
         level   = 0;
         dent    = '   ';
         margin  = '';
         buffer  = { };
         srcmap  = { };
      }, Emitter)
   end

   function Emitter:spawn()
      return Process.new(function(input)
         for unit in input do
            self:build(unit)
         end
         yield(self:output())
      end)
   end

   function Emitter:indent()
      self.level  = self.level + 1
      self.margin = string.rep(self.dent, self.level)
   end

   function Emitter:undent()
      self.level  = self.level - 1
      self.margin = string.rep(self.dent, self.level)
   end

   function Emitter:writeln(str)
      self.buffer[#self.buffer + 1] = (str or "").."\n"
      self.srcmap[#self.srcmap + 1] = self.ctx.line
   end

   function Emitter:write(str)
      self.buffer[#self.buffer + 1] = str
   end
   function Emitter:writefmt(fmt, ...)
      self:write(fmt:format(...))
   end
   function Emitter:writelist(node, ...)
      for i=1, #node do
         node[i]:accept(self, ...)
         if i < #node then
            self:write(',')
         end
      end
   end

   function Emitter:write_srcmap(idx)
      local buf = { }
      local seen = { }
      local prev
      for i=1, #self.srcmap do
         local line = self.srcmap[i]
         seen[i] = true
         buf[#buf + 1] = tostring(line)
      end
      for k,v in pairs(self.srcmap) do
         if not seen[k] then
            buf[#buf + 1] = k.."="..string.format("%q", v)
         end
      end
      local map = table.concat(buf, ",")
      self:write('require("blaze.core")._MAPS["'..self.unit.path..'"]={'..map..'};')
   end

   function Emitter:output()
      return table.concat(self.buffer)
   end

   function Emitter:build(unit)
      self.unit = unit
      self.srcmap = { }
      unit.tree:accept(self, unit)
   end

   function Emitter:visitNode(node)
      return self.ctx:sync_line(node.line)
   end

   function Emitter:visitNodeList(list, ...)
      for n in list:children() do
         self.ctx:sync_line(n:get_line())
         n:accept(self, ...)
      end
   end

   function Emitter:visitBlockNode(node, ...)
      for n in node.body:children() do
         self.ctx:sync_line(n:get_line())
         n:accept(self, ...)
         self:writeln(';')
      end
   end

   local DEBUG = false
   function Emitter:visitChunkNode(node, unit)
      self.ctx:sync_line(node:get_line())

      if false and not self.imports["blaze.core"] then
         local path = package.searchpath("blaze.core", package.path)
         local file = assert(io.open(path))
         local code = file:read("*a")
         file:close()
         self:write("package.preload['blaze.core']=")
         self:writefmt("loadstring(%s, '@%s');", util.quote(code), path)
         self.imports["blaze.core"] = path
      end

      --XXX: this isn't safely quoted - keep a per unit buffer and use
      -- util.quote instead (caveat: it'll be harder to read the
      -- generated output)
      self:writefmt("package.preload[%q]=assert(loadstring([=[",unit.path)
      self:writeln("local __core__ = require('blaze.core');")
      self:writeln("module('', __core__.environ);")

      self:visitBlockNode(node)
      self:writeln("return _M;")
      self:write("]=], '="..unit.path.."'));")
      self:write_srcmap()
      if unit.main then
         self:writefmt('require(%q)', unit.path)
      end
   end

   function Emitter:visitImportStatement(node)
      local path = node:get_path()
      local alias_list, names_list = { }, { }
      if #node.terms > 0 then
         for i=1, #node.terms do
            local t = node.terms[i]
            local n = t.name:get_symbol()
            local a
            if t.alias then
               if t.alias then
                  a = t.alias:get_symbol()
               else
                  a = n
               end
            end
            alias_list[#alias_list + 1] = a
            names_list[#names_list + 1] = string.format('%q', n)
         end
      else
         local unit = self.ctx.registry:get_info(node:get_path())
         for name in unit:exports() do
            alias_list[#alias_list + 1] = name
            names_list[#names_list + 1] = string.format('%q', name)
         end
      end
      self:write('local '..table.concat(alias_list, ',')..'=')
      self:writefmt('import(%q,%s)', path, table.concat(names_list, ','))
   end

   function Emitter:visitModuleDeclaration(node)

   end

   function Emitter:visitLiteral(node)
      self.ctx:sync_line(node:get_line())
      if type(node.value) == 'string' then
         self:write(util.quote(node.value))
      else
         self:write(tostring(node.value))
      end
   end

   function Emitter:visitIdentifier(node, bind)
      self.ctx:sync_line(node:get_line())
      local name = node:get_symbol()
      if name == '__FILE__' then
         return self:writefmt('%q', self.ctx.path)
      end
      if name == '__LINE__' then
         return self:writefmt(tostring(self.ctx.line))
      end
      local msym = util.mangle(name)
      if msym ~= name then
         self.srcmap[msym] = name
      end
      self:write(msym)
      if bind then
         self:write('='..bind)
      end
   end

   function Emitter:visitLocalDeclaration(node, scope)
      self.ctx:sync_line(node:get_line())
      self:write('local ')
      self:writelist(node.names, scope)
      if #node.inits > 0 then
         self:write('=')
         self:writelist(node.inits, scope)
      end
   end

   function Emitter:visitAssignExpression(node)
      self.ctx:sync_line(node:get_line())
      local temp_list = { }
      for i=1, #node.left do
         temp_list[i] = util.genid('__ref')
      end
      self:write('local '..table.concat(temp_list, ','))
      self:write('=')
      self:writelist(node.right)
      self:write(';')
      for i=1, #node.left do
         node.left[i]:accept(self, temp_list[i])
         if i < #node.left then
            self:write(';')
         end
      end
   end

   function Emitter:visitExpressionStatement(node, ...)
      self.ctx:sync_line(node.line)
      node:visit_children(self, ...)
      --self:writeln(';')
   end

   function Emitter:visitMemberExpression(node, bind, call)
      self.ctx:sync_line(node.line)
      self:write('(')
      node.object:accept(self)
      self:write(')')
      if call then
         if node.namespace then
            if node:is_computed() then
               self:write('[')
               node.property:accept(self)
               self:write(']')
            else
               self:write('.')
               node.property:accept(self)
            end
         else
            self:write(":")
            node.property:accept(self)
         end
      elseif bind then
         if node:is_computed() then
            if node.namespace then
               self:write('[')
               node.property:accept(self)
               self:write(']='..bind)
            else
               self:write(':__setindex(')
               node.property:accept(self)
               self:write(','..bind..')')
               self.srcmap['__setindex'] = '[]='
            end
         else
            local name = util.mangle(node.property:get_symbol())
            if node.namespace then
               self:write('.'..name..'='..bind)
            else
               self:write(':__set_'..name..'('..bind..')')
               self.srcmap['__set_'..name] = name..'='
            end
         end
      else
         if node:is_computed() then
            if node.namespace then
               self:write('[')
               node.property:accept(self)
               self:write(']')
            else
               self:write(':__getindex(')
               node.property:accept(self)
               self:write(')')
               self.srcmap['__getindex'] = '[]'
            end
         else
            local name = util.mangle(node.property:get_symbol())
            if node.namespace then
               self:write("."..name)
            else
               self:write(':__get_'..name..'()')
               self.srcmap['__get_'..name] = name
            end
         end
      end
   end

   function Emitter:visitTableLiteral(node, bind)
      self:write('Table({')
      for i=1, #node.entries do
         local n = node.entries[i]
         if n.tag == 'TableItem' then
            n.value:accept(self, bind)
         else
            n.name:accept(self, bind)
            self:write('=')
            if n.expr then
               self:write("[")
               n.expr:accept(self, bind)
               self:write("]")
            else
               n.value:accept(self, bind)
            end
         end
         self:write(';')
      end
      self:write('})')
   end

   function Emitter:visitRichString(node, ...)
      for i=1, #node.terms do
         local need_tostring = type(node.terms[i].value) ~= 'string'
         if need_tostring then
            self:write('tostring(')
         end
         node.terms[i]:accept(self)
         if need_tostring then
            self:write(')')
         end
         if i < #node.terms then
            self:write('..')
         end
      end
   end

   function Emitter:visitCallExpression(node, ...)
      self.ctx:sync_line(node:get_line())
      node.callee:accept(self, nil, true)
      self:write('(')
      self:writelist(node.arguments)
      self:write(')')
   end

   function Emitter:visitArrayPattern(node)
   end

   function Emitter:visitTablePattern(node)
   end

   function Emitter:visitApplyPattern(node)
   end

   function Emitter:visitInExpression(node)
   end

   function Emitter:visitUpdateExpression(node)
   end

   function Emitter:visitSelfExpression(node)
      return 'self'
   end

   function Emitter:visitSuperExpression(node)
      return 'super'
   end

   function Emitter:visitClassNode(node)
      local name = node.name:get_symbol()
      local info = node.info
      self:writefmt('%s=class(%q', name, name)
      self:writeln(',function(self, super)')

      -- default to `Dynamic` for type params
      self:write('self.__proto.__info={')
      for i=1, #info.params do
         self:write('Dynamic')
         if i < #info.params then
            self:write(',')
         end
      end
      self:writeln('};')

      node.body:accept(self, info)
      self:write('end')
      self:writeln(')')
   end

   local function write_param_inits(self, head)
      for p in head.params:children() do
         if p.init then
            local s = p.name:get_symbol()
            self:write("if "..s.." == nil then ")
            self:write(s.."=")
            p.init:accept(self)
            self:write(';')
            self:writeln(' end')
         end
      end
   end

   function Emitter:visitPropertyNode(node, info)
      local name = node.name:get_symbol()
      local vtab = 'self.__proto'
      if node.is_static then
         self:write('function self:__set_'..name..'(...)')
      else
         self:write('function '..vtab..':__set_'..name..'(...)')
      end
      if self.ctx:is_checked() and node.type then
         local type_name = node.type.base:get_symbol()
         for i=1, #info.params do
            if info.params[i] == type_name then
               self:writefmt(
                  '__check__(%q,...,self.__info[%s])', name, i
               )
               break
            end
         end
      end
      self:write(' self.'..name..'=...')
      self:writeln(' end')
      if node.is_static then
         self:write('function self:__get_'..name..'()')
      else
         self:write('function '..vtab..':__get_'..name..'()')
      end
      self:write(' return self.'..name)
      self:writeln(' end')
   end

   function Emitter:visitMethodNode(node)
      local name = node:get_name()
      local vtab = 'self.__proto'
      if node.is_static then
         self:write('function self:'..name..'(')
      else
         self:write('function '..vtab..':'..name..'(')
      end
      node.head:accept(self)
      self:writeln(')')
      write_param_inits(self, node.head)
      node.body:accept(self)
      self:writeln('end')
      if node.is_static then
         self:writeln('function self:__get_'..name..'()')
      else
         self:writeln('function '..vtab..':__get_'..name..'()')
      end
      self:writeln('return function(...) return self:'..name..'(...) end')
      self:writeln('end')
   end

   function Emitter:visitFunctionNode(node)
      if node:is_local() then
         self:write('local ')
      end
      if node:is_expression() then
         self:write('function(')
      else
         self:write('function ')
         node.name:accept(self)
         self:write('(')
      end
      node.head:accept(self)
      self:writeln(')')
      write_param_inits(self, node.head)
      node.body:accept(self)
      self:writeln(' end')
   end
   function Emitter:visitParameterList(list, ...)
      self:writelist(list, ...)
   end
   function Emitter:visitSignatureNode(node, ...)
      node.params:accept(self, ...)
   end
   function Emitter:visitParameterNode(node, ...)
      self:write(node.name:get_symbol())
   end
   function Emitter:visitReturnStatement(node, ...)
      self:write('return ')
      node.arguments:accept(self, ...)
   end
   function Emitter:visitNewExpression(node)
      self:write('new(')
      node.base:accept(self)
      if node.types then
         self:write(',')
         node.types:accept(self)
      else
         self:write(',nil')
      end
      if #node.arguments > 0 then
         self:write(',')
         self:writelist(node.arguments)
      end
      self:write(')')
   end
   function Emitter:visitTypeName(node)
      node:visit_children(self)
   end
   function Emitter:visitTypeList(node)
      self:write('{')
      self:writelist(node.elements)
      self:write('}')
   end
end

return Emitter
