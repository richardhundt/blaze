local scope = require("blaze.lang.scope")

-- holds mappings from nodes to their type info
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

-- unused? (scope is probably what you want) 
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

local UnionType = { kind = "UnionType" } do
   function UnionType.new(a, b)
      return setmetatable({ a, b }, UnionType)
   end
   function UnionType:is_compat(that)
      return self[1]:is_compat(that) or self[2]:is_compat(that)
   end
end

local NestedInfo = { kind = "nested" } do
   NestedInfo.__index = setmetatable(NestedInfo, TypeInfo)
   function NestedInfo:set_parent(parent)
      self.parent = parent
   end

   function NestedInfo:get_parent(kind)
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
   VarInfo.__index = setmetatable(VarInfo, NestedInfo)
   function VarInfo.new(name, type)
      return setmetatable({
         name = name,
         type = type or AnyType,
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
   function ParamInfo.new(...)
      return setmetatable(VarInfo.new(...), ParamInfo)
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
   FunctionInfo.__index = setmetatable(FunctionInfo, NestedInfo)
   function FunctionInfo.new(name, params, returns)
      return setmetatable({
         name = name,
         params = params or { },
         returns = returns or { },
      }, FunctionInfo)
   end

   function FunctionInfo:add_parameter(info)
      self.params[#self.params + 1] = info
   end
   function FunctionInfo:add_return(info)
      self.returns[#self.returns + 1] = info
   end
   function FunctionInfo:set_name(name)
      self.name = name
   end
end

local MethodInfo = { kind = "method" } do
   MethodInfo.__index = setmetatable(MethodInfo, FunctionInfo)
   function MethodInfo.new(...)
      return setmetatable(FunctionInfo.new(...), MethodInfo)
   end
end

local TraitInfo = { kind = "trait" } do
   TraitInfo.__index = setmetatable(TraitInfo, NestedInfo)
   function TraitInfo.new(name, base)
      return setmetatable({
         name      = name;
         base      = base;
         params    = { };
         methods   = { };
         fields    = { };
         mixins    = { };
         operators = { };
      }, TraitInfo)
   end
   function TraitInfo:has_parameters()
      return #self.params > 0
   end

   function TraitInfo:add_operator(oper, info)
      self.operators[oper] = info
      self.operators[#self.operators + 1] = oper
   end

   -- XXX: nominal only for now
   function TraitInfo:add_parameter(name, info)
      self.params[#self.params + 1] = name
      self.params[name] = #self.params
   end

   function TraitInfo:add_method(name, info)
      self.methods[name] = info
      self.methods[#self.methods + 1] = name
      info:set_parent(self)
   end
   function TraitInfo:find_method(name)
      if self.method[name] then
         return self.method[name]
      end
      if self.base then
         return self.base:find_method(name)
      end
   end
   function TraitInfo:add_field(name, info)
      self.fields[#self.fields + 1] = name
      self.fields[name] = info
   end
   function TraitInfo:find_field(name)
      if self.fields[name] then
         return self.fields[name]
      end
      if self.base then
         return self.base:find_field(name)
      end
   end
   function TraitInfo:method_pairs()
      local i = 0
      return function()
         i = i + 1
         local name = self.methods[i]
         if name then
            return name, self.methods[name]
         end
      end
   end
   function TraitInfo:field_pairs()
      local i = 0
      return function()
         i = i + 1
         local name = self.fields[i]
         if name then
            return name, self.fields[name]
         end
      end
   end
   function TraitInfo:computed_field_list()
      local field_list = { }
      local super_list = { }
      local curr = self
      while curr do
         super_list[#super_list + 1] = curr
         curr = curr.base
      end
      for i=#super_list, 1, -1 do
         local base = super_list[i]
         for i=1, #base.fields do
            field_list[#field_list + 1] = base.fields[base.fields[i]]
         end
      end
      return field_list
   end
   function TraitInfo:add_mixin(that)
      self.mixins[that] = true
      self.mixins[#self.mixins + 1] = that
      for name, info in that:method_pairs() do
         self:add_method(name, info)
      end
      for name, info in that:field_pairs() do
         self:add_field(name, info)
      end
   end
   function TraitInfo:set_base(that)
      self.base = that
   end
   function TraitInfo:field_index(name)
      local field_list = self:computed_field_list()
      for i=1, #field_list do
         if field_list[i].name == name then
            return i
         end
      end
   end
   function TraitInfo:is_compat(that)
      if self == that then
         return true
      end
      if self.mixins[that] then
         return true
      end
      return false
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
         imports = { }; -- imported scopes (ordered)
         entries = { }; -- locally defined names
         buffer  = { };
         srcmap  = { };
      }, Unit)
   end
   function Unit:set_module(module)
      self.module = module
   end
   function Unit:get_module()
      return self.module
   end
   function Unit:define(name, info)
      self.entries[name] = info
      return self.module:define(name, info)
   end
   function Unit:lookup(name)
      if self.entries[name] then
         return self.entries[name]
      end
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
   function Unit:exports()
      return pairs(self.entries)
   end
end

-- A module is a named scope and holds a symbol table with all the top level declarations.
-- The symbol table is a nested scope object for now with CORE as its outer scope.
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

   function Module:get_name()
      return self.name
   end

end

-- root container for module namespaces
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

local ValueType = { kind = "ValueType" } do
   ValueType.__index = ValueType
   function ValueType:add_operator(oper, desc)
      self.members[oper] = desc
   end
   function ValueType:get_member(name)
      return self.members[name]
   end
end

local AnyType = { kind = "AnyType", name = "Any" } do
   setmetatable(AnyType, ValueType)
   AnyType.members = { }
   function AnyType:is_compat(that)
      return true
   end
end

local NilType = { kind = "NilType", name = "Nil" } do
   setmetatable(NilType, ValueType)
   NilType.members = { }
   function NilType:is_compat(that)
      if that == NilType then
         return true
      end
   end
end

local NullType = { kind = "NullType", name = "Null" } do
   setmetatable(NullType, ValueType)
   NullType.members = { }
   function NullType:is_compat(that)
      return that.kind == "NullType" or that.kind == "AnyType"
   end
end

local NumberType = { kind = "NumberType", name = "Number" } do
   setmetatable(NumberType, ValueType)

   local unary_arith = {
      kind = "operator",
      params = { NumberType },
      returns = { NumberType }
   }

   local infix_arith = {
      kind = "operator",
      params = { NumberType, NumberType },
      returns = { NumberType }
   }

   NumberType.members = {
      ['-_'] = unary_arith,
      ['~_'] = unary_arith,
      ['+'] = infix_arith,
      ['-'] = infix_arith,
      ['/'] = infix_arith,
      ['*'] = infix_arith,
      ['%'] = infix_arith,
      ['**'] = infix_arith,
      ['^'] = infix_arith,
      ['&'] = infix_arith,
      ['|'] = infix_arith,
      ['<<'] = infix_arith,
      ['>>'] = infix_arith,
      ['>>>'] = infix_arith,
   }

   function NumberType:is_compat(that)
      return that.kind == "NumberType" or that.kind == "AnyType"
   end
end

local StringType = { kind = "StringType", name = 'String' } do
   setmetatable(StringType, ValueType)
   StringType.members = {
      ["~"] = {
         kind = "operator",
         params = { StringType, StringType },
         returns = { StringType }
      },
      ["sub"] = {
         kind = "method",
         params = { NumberType, NumberType },
         returns = { StringType }
      }
   }
   function StringType:is_compat(that)
      return that == StringType or that == NumberType or that == AnyType
   end
end

local BooleanType = { kind = "BooleanType",  base = ValueType }

local TableType = { kind = "TableType",    base = ValueType } do

end

local FunctionType = { kind = "FunctionType", base = ValueType }
local ArrayType    = { kind = "ArrayType",    base = ValueType }
local ClassType    = { kind = "ClassType",    base = ValueType }
local TraitType    = { kind = "TraitType",    base = ValueType }
local VoidType     = { kind = "VoidType" }

return {
   Registry = Registry,
   Universe = Universe,
   Unit = Unit,
   Module = Module,

   NumberType = NumberType,
   StringType = StringType,

   VarInfo = VarInfo,
   ParamInfo = ParamInfo,
   FunctionInfo = FunctionInfo,
   MethodInfo = MethodInfo,
   TraitInfo = TraitInfo,
   ClassInfo = ClassInfo,
}

