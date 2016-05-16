local util    = require("blaze.lang.util")
local tree    = require("blaze.lang.tree")
local scope   = require("blaze.lang.scope")
local model   = require("blaze.lang.model")
local Process = require("blaze.lang.process")


local Checker = { } do
   Checker.__index = setmetatable(Checker, tree.Visitor)

   local yield = coroutine.yield

   function Checker.new(ctx)
      return setmetatable({ ctx = ctx }, Checker)
   end

   function Checker:spawn()
      return Process.new(function(input)
         for unit in input do
            self.ctx:set_file(unit.path)
            xpcall(function()
               self:analyze(unit)
            end, function(e)
               print(e, debug.traceback())
            end)
            yield(unit)
         end
      end)
   end

   function Checker:analyze(unit)
      unit.tree:accept(self, unit)
   end

   function Checker:visitNode(node)
      self.ctx:sync_line(node:get_line())
   end

   function Checker:visitNodeList(list, ...)
      return list:visit_children(self, ...)
   end

   function Checker:visitChunkNode(node, ...)
      node:visit_children(self, ...)
   end

   function Checker:visitBlockNode(node, outer)
      local scope = scope.NestedScope.new(outer, "block")
      node:visit_children(self, scope)
      return scope
   end

   function Checker:visitFunctionNode(node, outer)
      local head = node.head
      local scope = scope.NestedScope.new(outer, "function")
      local params, returns
      if head.params then
         params = head.params:accept(self, scope)
      end
      if head.returns then
         returns = head.returns:accept(self, scope)
      end
      local info = model.FunctionInfo.new(nil, params, returns)
      node.info = info
      if not node:is_expression() then
         local name = node.name:get_symbol()
         info:set_name(name)
         outer:define(name, info)
      end
      node.body:accept(self, scope)
      return info
   end

   function Checker:type_error(expect, got)
      self.ctx:abort(string.format("type error: expected %s got %s\n%s", expect.name, got.name, debug.traceback()))
   end

   function Checker:visitAssignExpression(node, scope)
      local want = { }
      for i=1, #node.left do
         local lhs = node.left[i]
         if lhs.tag == 'Identifier' then
            local lhs_info = scope:lookup(lhs:get_symbol())
            if not lhs_info then
               self.ctx:abort(string.format("%s is not defined", lhs:get_symbol()))
            end
            want[#want + 1] = lhs_info.type
         elseif lhs.tag == "TablePattern" then

         elseif lhs.tag == "ArrayPattern" then

         elseif lhs.tag == "ApplyPattern" then

         else
            assert(lhs.tag == "MemberExpression")
         end
      end

      local have = { }
      for i=1, #node.right do
         local rhs = node.right[i] or model.AnyType
         local rhs_type = rhs:accept(self, scope)
         have[#have + 1] = rhs_type
      end

      for i=1, #want do
         if not want[i]:is_compat(have[i]) then
            self:type_error(want[i], have[i])
         end
      end
   end

   function Checker:visitClassNode(node, parent)
      local scope = scope.NestedScope.new(outer, "class")
      return node:visit_children(self, scope)
   end

   function Checker:visitLocalDeclaration(node, scope)
      local want = { }
      for i=1, #node.names do
         local n = node.names[i]
         if n.tag == 'Identifier' then
            local name = n:get_symbol()
            local info = model.VarInfo.new(name)
            if n.type then
               print("Identifier TYPE:", util.dump(n.type))
               info.type = n.type:accept(self, scope)
               print("RESOLVED:", util.dump(info.type))
            else
               info.type = model.AnyType
            end
            scope:define(name, info)
            want[#want + 1] = info.type
         elseif n.tag == 'TablePattern' then
            error("destructure table NYI")
         elseif n.tag == 'ArrayPattern' then
            error("destructure array NYI")
         elseif n.tag == 'CallExpression' then
            error("destructure call NYI")
         end
      end
      local have = { }
      for i=1, #node.inits do
         local n = node.inits[i]
         local l = node.inits[i]:accept(self, scope)
         --for j=1, #l do
            have[#have + 1] = l
         --end
      end
      for i=1, #want do
         if not want[i]:is_compat(have[i]) then
            self:type_error(want[i], have[i])
         end
      end
   end

   function Checker:visitTypeName(node, scope)
      local name = node.base:get_symbol()
      local info = scope:lookup(name)
      if not info then
         self.ctx:abort("no such type: "..name)
      end
      if node.arguments then
         local args = { }
         for i=1, #node.arguments.elements do
            local a = node.arguments.elements[i]:accept(self, scope)
            args[#args + 1] = a
         end
         info = info:construct(args)
         print("ARGS...", util.dump(args))
      end
      print("visitTypeName returning:", util.dump(info))
      return info
   end

   function Checker:visitMemberExpression(node, scope, is_call)
      -- get the type of the object
      local obj_info = node.object:accept(self, scope, is_call)
      local att_name = node.property:get_symbol()
      -- get the type of the property
      local found
      if is_call then
         found = obj_info.type:find_method(att_name)
      else
         found = obj_info.type:find_field(att_name)
      end
      if not found then
         self.ctx:abort("no such member "..att_name)
      end
      return found
   end

   function Checker:visitRichString(node, ...)
      return model.StringType
   end

   function Checker:visitLiteral(node)
      local t = type(node.value)
      if t == 'string' then
         return model.StringType
      elseif t == 'number' then
         return model.NumberType
      elseif t == 'nil' then
         return model.NilType
      elseif t == 'boolean' then
         return model.BooleanType
      else
         error("NYI: Literal: " .. t)
      end
   end

   function Checker:visitExpression(node, ...)
      return node:visit_children(self, ...)
   end

   function Checker:visitBinaryExpression(node, ...)
      self:visitNode(node)

      local oper, lhs, rhs = node.operator, node.left, node.right
      local lhs_type = lhs:accept(self, ...)
      local rhs_type = rhs:accept(self, ...)

      if #lhs_type > 0 then
         lhs_type = lhs_type[1]
      end
      if #rhs_type > 0 then
         rhs_type = rhs_type[1]
      end

      local op_sig = lhs_type:get_member(oper)
      if op_sig then
         local p1_type = op_sig.params[1]
         local p2_type = op_sig.params[2]
         if not p1_type:is_compat(lhs_type) then
            self:type_error(p1_type, lhs_type)
         end
         if not p2_type:is_compat(rhs_type) then
            self:type_error(p2_type, rhs_type)
         end
         return op_sig.returns[1]
      else
         self.ctx:abort(string.format("operation '%s' not supported", oper))
      end
   end

   function Checker:visitExpressionStatement(node, ...)
      return node:visit_children(self, ...)
   end
   
   function Checker:visitNewExpression(node, scope)
      return scope:lookup(node.base.name)
   end

   function Checker:visitCallExpression(node, scope)
      local cinfo = node.callee:accept(self, scope, true)
      local pinfo = { }
      for i=1, #node.arguments do
         pinfo[#pinfo + 1] = node.arguments[i]:accept(self, scope)
      end
      if cinfo.params then
         if #pinfo ~= #cinfo.params then
            if not (#pinfo > #cinfo.params and cinfo.params[#cinfo.params]:is_rest()) then
               self.ctx:abort(string.format("wrong number of arguments (%s expected got %s)", #cinfo.params, #pinfo))
            end
         end
         for i=1, #pinfo do
            if i >= #cinfo.params and cinfo.params[i]:is_rest() then
               if not cinfo.params[#cinfo.params]:check_type(pinfo[i]) then
                  self:type_error(cinfo.params[#cinfo.params], pinfo[i])
               end
            else
               if not cinfo.params[i]:check_type(pinfo[i]) then
                  self:type_error(cinfo.params[i], pinfo[i])
               end
            end
         end
      end
   end

   function Checker:visitIdentifier(node, scope)
      local name = node:get_symbol()
      local info = scope:lookup(name)
      print("name:", name, "info:", util.dump(info))
      if not info and node:is_lookup() then
         self.ctx:abort("variable "..name.." is not defined")
      end
      return info
   end

end

return Checker
