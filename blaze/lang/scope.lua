local Scope = { kind = "scope" } do
   Scope.__index = Scope
   function Scope.new(kind)
      return setmetatable({
         kind = kind;
         entries = { };
      }, Scope)
   end

   function Scope:define(name, info)
      self.entries[name] = info
   end

   function Scope:lookup(name)
      return self.entries[name]
   end

   function Scope:export_to(other)
      for name, info in pairs(self.entries) do
         other:define(name, info)
      end
   end

   function Scope:import_from(other)
      other:export_to(self)
   end
end

local CORE = Scope.new("global")

local NestedScope = { kind = "nested" } do
   local super = Scope
   NestedScope.__index = setmetatable(NestedScope, super)

   function NestedScope.new(outer, kind)
      local self = setmetatable(super.new(kind), NestedScope)
      self.outer = outer
      return self
   end

   function NestedScope:lookup(name, is_local)
      local info = super.lookup(self, name)
      if info then
         return info, self
      elseif not is_local and self.outer then
         return self.outer:lookup(name)
      else
         return nil
      end
   end

   function NestedScope:local_lookup(name)
      return self:lookup(name, true)
   end

   function NestedScope:get_outer(kind)
      local outer = self.outer
      if not kind then
         return outer
      end
      while outer do
         if outer.kind == kind then
            return outer
         end
         outer = outer.outer
      end
   end

end

return {
   CORE = CORE,
   Scope = Scope,
   NestedScope = NestedScope,
}

