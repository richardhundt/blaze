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
      self:writeln('__core__._MAPS["'..self.unit.path..'"]={'..map..'};')
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
      for n in node:children() do
         self.ctx:sync_line(n:get_line())
         n:accept(self, ...)
         self:writeln(';')
      end
   end

   local DEBUG = false
   function Emitter:visitChunkNode(node, unit)
      self.ctx:sync_line(node:get_line())

      if false and not unit.imports["blaze.core"] then
         local path = package.searchpath("blaze.core", package.path)
         local file = assert(io.open(path))
         local code = file:read("*a")
         file:close()
         code = string.dump(loadstring(code, "@"..path), not DEBUG)

         self:writefmt("package.preload['blaze.core'] = loadstring(%s, '@%s')", util.quote(code), path)
         self:writeln(";")
         unit.imports["blaze.core"] = path
      end

      self:write("local __unit__ = loadstring([[")
      self:writeln("local __core__ = require('blaze.core');")
      self:writeln("module('', __core__.environ);")
      for n in node.body:children() do
         self.ctx:sync_line(n:get_line())
         n:accept(self)
         self:writeln(";")
      end
      self:write_srcmap()
      self:write("]], '="..unit.path.."');")

      self:writefmt("require('blaze.core').run(__unit__);")
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
      local temp_list = { }
      for i=1, #node.left do
         temp_list[i] = util.genid('__ref')
      end
      self:write('local '..table.concat(temp_list, ','))
      self:write('=')
      self:writelist(node.right)
      self:writeln(';')
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
               self:writeln(','..bind..')')
               self.srcmap['__setindex'] = '[]='
            end
         else
            local name = util.mangle(node.property:get_symbol())
            if node.namespace then
               self:writeln('.'..name..'='..bind)
            else
               self:writeln(':__set_'..name..'('..bind..')')
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
      node.body:accept(self, info)
      self:write('end')
      self:writeln(')')
   end

   local function write_param_inits(self, head)
      for p in head.params:children() do
         if p.init then
            local s = p.name:get_symbol()
            self:writeln("if "..s.." == nil then")
            self:write(s.."=")
            p.init:accept(self)
            self:writeln(';')
            self:writeln('end')
         end
      end
   end

   function Emitter:visitPropertyNode(node, info)
      local name = node.name:get_symbol()
      local vtab = 'self.__proto'
      self:write('function '..vtab..':__set_'..name..'(...)')
      if self.ctx:is_checked() and node.type then
         local type_name = node.type.base:get_symbol()
         if info:has_parameters() then
            for i=1, #info.params do
               if info.params[i] == type_name then
                  self:writefmt(
                     '__check__(%q,...,self.__type_info[%s])',name, i
                  )
                  break
               end
            end
         end
      end
      self:write(' self.'..name..'=...')
      self:writeln(' end')
      self:write('function '..vtab..':__get_'..name..'()')
      self:write(' return self.'..name)
      self:writeln(' end')
   end

   function Emitter:visitMethodNode(node)
      local name = node:get_name()
      local vtab = 'self.__proto'
      self:write('function '..vtab..':'..name..'(')
      node.head:accept(self)
      self:writeln(')')
      write_param_inits(self, node.head)
      node.body:accept(self)
      self:writeln('end')
      self:writeln('function '..vtab..':__get_'..name..'()')
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
   function Emitter:visitRepeatStatement(node, ...)
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
