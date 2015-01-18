local tree    = require("blaze.lang.tree")
local scope   = require("blaze.lang.scope")
local model   = require("blaze.lang.scope")
local Process = require("blaze.lang.process")


local Checker = { } do
   Checker.__index = setmetatable(Checker, tree.Visitor)

   local yield = coroutine.yield
   local util  = require("blaze.lang.util")

   function Checker.new(ctx)
      local self = setmetatable({ ctx = ctx }, Checker)
      self.scope = scope.Scope.new(scope.CORE)
      return self
   end

   function Checker:spawn()
      return Process.new(function(input)
         for unit in input do
            self.ctx:set_file(unit.path)
            self:analyze(unit)
            yield(unit)
         end
      end)
   end

   function Checker:begin_scope(kind)
      self.scope = scope.NestedScope.new(self.scope, kind)
      return self.scope
   end

   function Checker:end_scope()
      local outer = self.scope
      self.scope = outer.outer
      return outer
   end

   function Checker:analyze(unit)
      unit.tree:accept(self, unit)
   end

   function Checker:visitNode(node)
      self.ctx:sync_line(node:get_line())
   end

   function Checker:visitNodeList(list, ...)
      list:visit_children(self, ...)
   end

   function Checker:visitChunkNode(node, ...)
      node:visit_children(self, ...)
   end

   function Checker:visitBlockNode(node, outer)
      local scope = self:begin_scope("block")
      node:visit_children(self, scope)
      self:end_scope()
   end

   function Checker:visitClassNode(node, parent)
      print("GOT INFO:", util.dump(node.info))
   end

   function Checker:visitLocalDeclaration(node, parent)

   end

   function Checker:visitIdentifier(node, parent)
      local name = node:get_symbol()
      if node:is_lookup() then

      end
   end
end

return Checker
