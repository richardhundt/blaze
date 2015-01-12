local function push(op, val)
   op[#op + 1] = val
end

local Op = { }
setmetatable(Op, {
   __call = function(Op, ...)
      if select('#', ...) == 0 then
         return setmetatable({ }, Op)
      end
      local v = ...
      if type(v) ~= "table" then
         error("invalid op descriptor")
      end
      return setmetatable(v, Op)
   end
})
Op.__index = {
   push = push,
   write = function(self, writer)
      return writer:op(self)
   end
}

local OpConst = { }
setmetatable(OpConst, {
   __call = function(OpConst, ...)
      assert(select('#', ...) == 1, "value expected")
      return setmetatable({ (...) }, OpConst)
   end
})
OpConst.__index = {
   write = function(self, writer)
      return writer:const(self)
   end
}

local OpList = { }
setmetatable(OpList, {
   __call = function(mt, t)
      return setmetatable(t or { }, mt)
   end
})
OpList.__index = {
   push = push,
   write = function(self, writer)
      return writer:list(self)
   end
}

local OpChunk = { }
setmetatable(OpChunk, {
   __call = function(mt, t)
      return setmetatable(t or { }, mt)
   end
})
OpChunk.__index = {
   push = push,
   write = function(self, writer)
      return writer:chunk(self)
   end
}

Op.const = OpConst
Op.chunk = OpChunk
Op.list  = OpList

return Op

