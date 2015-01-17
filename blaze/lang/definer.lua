local util    = require("blaze.lang.util")
local Process = require("blaze.lang.process")
local Queue   = require("blaze.lang.queue")
local tree    = require("blaze.lang.tree")
local model   = require("blaze.lang.model")
local scope   = require("blaze.lang.scope")

do
   --[[
   local predef = {
      'Nil','Number','Boolean', 'String', 'Function', 'Coroutine', 'Range',
      'UserData', 'Table', 'Array', 'Error', 'Trait', 'Class', 'Object',
      'Grammar', 'Pattern', 'ArrayPattern', 'TablePattern', 'ApplyPattern',
      '__magic__', 'yield', 'take', 'typeof', 'null', 'warn', 'eval',
      'any', 'all', '__FILE__', '__LINE__', '_M', '_PACKAGE', '_NAME',
      'Meta', 'Any', 'All'
   }

   for k,v in pairs(_G) do
      scope.CORE:define(k, { line = 0 })
   end

   for i=1, #predef do
      scope.CORE:define(predef[i], { line = 0 })
   end
   --]]
end

local Definer = { } do
   Definer.__index = setmetatable(Definer, tree.Visitor)

   local yield = coroutine.yield

   function Definer.new(ctx)
      local self = setmetatable({ ctx = ctx }, Definer)
      self.loaded = { }
      self.queue  = { }
      self.module = ctx.universe:get_module("")
      return self
   end

   function Definer:spawn()
      return Process.new(function(input)
         self.input = input
         for unit in input do
            local save_unit   = self.unit
            local save_module = self.module

            self:resolve(unit)

            self.unit   = save_unit
            self.module = save_module
         end
         for i=#self.queue, 1, -1 do
            yield(self.queue[i])
         end
      end)
   end

   function Definer:add_import(path)
      if not self.loaded[path] then
         self.loaded[path] = true
         local item = {
            path = path,
            from = {
               line = self.ctx:get_line(),
               file = self.ctx:get_file(),
            }
         }
         self.ctx.reader:enqueue(item)
      end
   end

   function Definer:resolve(unit)
      print(tree.Dumper.dump(unit.tree))
      self.ctx:set_file(unit.path)

      self.loaded[unit.path] = unit
      self.queue[#self.queue + 1] = unit

      self.ctx.registry:set_info(unit.path, unit)
      self.unit = unit

      unit.tree:accept(self)

      return self.module
   end

   function Definer:visitNode(node)
      self.ctx:sync_line(node:get_line())
   end

   function Definer:visitNodeList(node, ...)
      node:visit_children(self, ...)
   end

   function Definer:visitChunkNode(node)
      self:visitNode(node)
      self.ctx.registry:set_info(node, self.module)

      for n in node:children() do
         n:accept(self, self.module)
      end
   end

   function Definer:visitImportStatement(node)
      self:visitNode(node)

      local ok, path = pcall(node.get_path, node)
      if not ok then
         self.ctx:abort("import path not a constant string")
      end
      self:add_import(path)

      local unit = self.input()
      self.unit:add_import(self:resolve(unit))
   end

   function Definer:visitModuleStatement(node)
      self:visitNode(node)
      local name = node:get_name()

      local module = self.ctx.universe:get_module(name)
      if not module then
         module = model.Module.new(name)
         self.ctx.universe:add_module(name, module)
      end

      self.unit:set_module(module)
      self.ctx.registry:set_info(node, module)
   end

   function Definer:visitTraitNode(node, parent)
      self:visitNode(node)
      local name = node:get_name()
      local trait = model.TraitInfo.new(name)
      self.ctx.registry:set_info(node, trait)

      parent:define(name, trait)

      for n in node:children() do
         n:accept(self, trait)
      end
   end

   function Definer:visitClassNode(node, parent)
      self:visitNode(node)
      local name = node:get_name()
      local class = model.ClassInfo.new(name)
      self.ctx.registry:set_info(node, class)

      parent:define(name, class)

      for n in node:children() do
         n:accept(self, class)
      end
   end

   function Definer:visitMethodNode(node, parent)
      self:visitNode(node)
      local name = node:get_name()
      local method = model.MethodInfo.new(name)
      self.ctx.registry:set_info(node, method)

      parent:add_member(name, method)

      for n in node:children() do
         n:accept(self, method)
      end
   end

   function Definer:visitFunctionNode(node, parent)
      self:visitNode(node)
      local name
      if node.name.tag == 'Identifier' then
         name = node:get_name()
      end
      local func = model.FunctionInfo.new(name)
      self.ctx.registry:set_info(node, func)

      if name and not (node:is_local() or node:is_expression()) then
         parent:define(name, func)
      end

      for n in node:children() do
         n:accept(self, func)
      end
   end

   function Definer:visitParameterList(node, ...)
      node:visit_children(self, ...)
   end

   function Definer:visitParameterNode(node, parent)
      self:visitNode(node)
      local name  = node:get_name()
      local param = model.ParamInfo.new(name)

      param:set_type(node:get_type())
      param:set_init(node:get_init())
      param:set_rest(node:is_rest())

      parent:add_parameter(name, param)
   end

   function Definer:visitPropertyNode(node, ...)
      self:visitNode(node, ...) -- TODO
   end

   function Definer:visitBlockNode(node, ...)
      node:visit_children(self, ...)
   end

end

return Definer
