local scope = require("blaze.lang.scope")

local Registry = { } do
   Registry.__index = Registry
   function Registry.new()
      return setmetatable({
         mapping = { }
      }, Registry)
   end
   function Registry:set_info(node, info)
      self.mapping[node] = info
   end
   function Registry:get_info(node)
      return self.mapping[node]
   end
end

local Environ = { } do
   Environ.__index = Environ
   function Environ.new()
      return setmetatable({
         entries = { }
      }, Environ)
   end
end

local TypeInfo = { kind = "type" } do
   TypeInfo.__index = TypeInfo
   function TypeInfo.new()
      return setmetatable({ }, TypeInfo)
   end
end

local ValueType = { kind = "ValueType" } do
   ValueType.__index = ValueType
   function ValueType:add_operator(oper, desc)
      self.members[oper] = desc
   end
end

local AnyType = { kind = "AnyType" } do
   function AnyType:is_compat(that)
      return true
   end
end

local NilType = { kind = "NilType" } do
   function NilType:is_compat(that)
      if that == NilType then
         return true
      end
   end
end

local NullType = { kind = "NullType" } do
   function NullType:is_compat(that)
      return that.kind == "NullType" or that.kind == "AnyType"
   end
end

local NumberType = { kind = "NumberType" } do
   setmetatable(NumberType, ValueType)

   NumberType.members = { }

   NumberType:add_operator("-_", {
      native  = true,
      params  = { NumberType },
      returns = { NumberType }
   })

   NumberType:add_operator("+", {
      native  = true,
      params  = { NumberType, NumberType },
      returns = { NumberType }
   })

   NumberType:add_operator("-", {
      native  = true,
      params  = { NumberType, NumberType },
      returns = { NumberType }
   })

   NumberType:add_operator("*", {
      native  = true,
      params  = { NumberType, NumberType },
      returns = { NumberType }
   })

   NumberType:add_operator("/", {
      native  = true,
      params  = { NumberType, NumberType },
      returns = { NumberType }
   })

   NumberType:add_operator("%", {
      native  = true,
      params  = { NumberType, NumberType },
      returns = { NumberType }
   })

   NumberType:add_operator("^", {
      native  = true,
      params  = { NumberType, NumberType },
      returns = { NumberType }
   })

   function NumberType:is_compat(that)
      return that.kind == "NumberType" or that.kind == "AnyType"
   end
end

local StringType   = { kind = "StringType",   base = ValueType }
local BooleanType  = { kind = "BooleanType",  base = ValueType }
local FunctionType = { kind = "FunctionType", base = ValueType }
local TableType    = { kind = "TableType",    base = ValueType }
local ArrayType    = { kind = "ArrayType",    base = ValueType }
local ClassType    = { kind = "ClassType",    base = ValueType }
local TraitType    = { kind = "TraitType",    base = ValueType }
local VoidType     = { kind = "VoidType" }

local UnionType = { kind = "UnionType" } do
   function UnionType.new(a, b)
      return setmetatable({ a, b }, UnionType)
   end
   function UnionType:is_compat(that)
      return self[1]:is_compat(that) or self[2]:is_compat(that)
   end
end

local Nested = { kind = "nested" } do
   Nested.__index = setmetatable(Nested, TypeInfo)
   function Nested:set_parent(parent)
      self.parent = parent
   end

   function Nested:get_parent(kind)
      local parent = self.parent
      if not kind then
         return parent
      end
      while parent do
         if parent.kind == kind then
            return parent
         end
         parent = parent.parent
      end
   end
end

local VarInfo = { kind = "variable" } do
   VarInfo.__index = setmetatable(VarInfo, Nested)
   function VarInfo.new(name)
      return setmetatable({
         name = name,
         type = AnyType,
      }, VarInfo)
   end
   function VarInfo:set_type(type)
      self.type = type
   end
   function VarInfo:check_type(that)
      if self.type == AnyType then
         return true
      end
      if that == AnyType then
         return true
      end
      return self.type:is_compat(that)
   end
end

local ParamInfo = { kind = "parameter" } do
   ParamInfo.__index = setmetatable(ParamInfo, VarInfo)
   function ParamInfo.new(name)
      return setmetatable(VarInfo.new(name), ParamInfo)
   end

   function ParamInfo:set_type(expr)
      self.type = expr
   end
   function ParamInfo:set_init(expr)
      self.init = expr
   end
   function ParamInfo:get_init()
      return self.init
   end
   function ParamInfo:set_rest(v)
      self.rest = v
   end
   function ParamInfo:get_rest()
      return self.rest
   end
   function ParamInfo:is_rest()
      return self.rest ~= nil
   end
end

local FunctionInfo = { kind = "function" } do
   FunctionInfo.__index = setmetatable(FunctionInfo, Nested)
   function FunctionInfo.new(name)
      return setmetatable({
         name = name,
         params = { },
         returns = { },
      }, FunctionInfo)
   end

   function FunctionInfo:add_parameter(info)
      self.params[#self.params + 1] = info
   end
   function FunctionInfo:add_return(info)
      self.returns[#self.returns + 1] = info
   end
end

local MethodInfo = { kind = "method" } do
   MethodInfo.__index = setmetatable(MethodInfo, FunctionInfo)
   function MethodInfo.new(name)
      return setmetatable(FunctionInfo.new(name), MethodInfo)
   end
end

local TraitInfo = { kind = "trait" } do
   TraitInfo.__index = setmetatable(TraitInfo, Nested)
   function TraitInfo.new(name, base)
      return setmetatable({
         name    = name;
         base    = base;
         params  = { };
         members = { };
         mixins  = { };
      }, TraitInfo)
   end

   function TraitInfo:add_param(info)
      self.params[#self.params + 1] = info
   end
   function TraitInfo:add_member(name, info)
      self.members[name] = info
      self.members[#self.members + 1] = name
      info:set_parent(self)
   end
   function TraitInfo:find_member(name)
      if self.members[name] then
         return self.members[name]
      end
      if self.base then
         return self.base:find_member(name)
      end
   end
   function TraitInfo:member_pairs()
      local i = 0
      return function()
         i = i + 1
         local name = self.members[i]
         if name then
            return name, self.members[name]
         end
      end
   end
   function TraitInfo:add_mixin(other)
      self.mixins[other] = true
      for name, info in other:member_pairs() do
         self:add_member(name, info)
      end
   end
end

local ClassInfo = { kind = "class" } do
   ClassInfo.__index = setmetatable(ClassInfo, TraitInfo)
   function ClassInfo.new(name, base)
      return setmetatable(TraitInfo.new(name, base), ClassInfo)
   end
end

-- A compilation unit. Keeps a path and ast root reference and
-- tracks which module to add members to during definition.
local Unit = { } do
   Unit.__index = Unit
   function Unit.new(path, tree)
      return setmetatable({
         path    = path;
         tree    = tree;
         imports = { };
      }, Unit)
   end
   function Unit:set_module(module)
      self.module = module
   end
   function Unit:get_module()
      return self.module
   end
   function Unit:define(name, info)
      return self.module:define(name, info)
   end
   function Unit:lookup(name)
      local info = self.module:lookup(name)
      if info then
         return info
      end
      for i=1, #self.imports do
         info = self.imports[i]:lookup(name, true)
         if info then
            return info
         end
      end
   end
   function Unit:add_import(scope)
      self.imports[#self.imports + 1] = scope
   end
end

-- A module holds a symbol table with all the top level declarations.
-- The symbol table is a nested scope object for now with CORE as its
-- outer scope.
local Module = { } do
   Module.__index = Module

   function Module.new(name)
      return setmetatable({
         name    = name;
         environ = scope.NestedScope.new(scope.CORE, "module");
      }, Module)
   end

   function Module:define(name, info)
      return self.environ:define(name, info)
   end

   function Module:lookup(name)
      return self.environ:lookup(name)
   end

   function Module:get_scope()
      return self.environ
   end
end

local Universe = { } do
   Universe.__index = Universe
   function Universe.new()
      return setmetatable({
         modules = { [""] = Module.new("") };
      }, Universe)
   end

   function Universe:add_module(name, module)
      self.modules[name] = module
   end
   function Universe:get_module(name)
      return self.modules[name]
   end
end

return {
   Registry = Registry,
   Universe = Universe,
   Unit = Unit,
   Module = Module,
   VarInfo = VarInfo,
   ParamInfo = ParamInfo,
   FunctionInfo = FunctionInfo,
   MethodInfo = MethodInfo,
   TraitInfo = TraitInfo,
   ClassInfo = ClassInfo,
}

