--[=[
Copyright (C) 2013-2014 Richard Hundt and contributors.
See Copyright Notice in blaze
]=]

local ffi  = require('ffi')
local lpeg = require('lpeg')
local null = ffi.cast('void*', 0x0)

local Class
local Range
local Object

local type, tonumber, tostring = _G.type, _G.tonumber, _G.tostring
local getmetatable, setmetatable = _G.getmetatable, _G.setmetatable
local pcall, unpack, select = _G.pcall, _G.unpack, _G.select

local __is__, __match__, include

local Meta = { }
Meta.__call = function(meta, ...)
   if meta.__apply then
      return meta.__apply(meta, ...)
   elseif meta.self then
      local self = setmetatable({ }, meta)
      meta.self(self, ...)
      return self
   end
end
Meta.__index = Meta
Meta.__proto = { }
Meta.__getindex = rawget
Meta.__setindex = rawset
Meta.__tostring = function(o)
   if o.toString then return o:toString() end
   return tostring(rawget(o, '__name') or type(o))
end

local Dynamic = setmetatable({ __name = 'Dynamic' }, Meta)

local Function = setmetatable({ __name = 'Function' }, Meta)
Function.__tostring = function(self)
   local info = debug.getinfo(self, 'un')
   local nparams = info.nparams
   local params = {}
   for i = 1, nparams do
      params[i] = debug.getlocal(self, i)
   end
   if info.isvararg then params[#params+1] = '...' end
   return string.format('function(%s): %p', table.concat(params,', '), self)
end
function Function.__index(self, k)
   return Function.__proto[k]
end
Function.__proto.clone = function(self)
   local copy = loadstring(string.dump(self))
   local info = debug.getinfo(self, 'u')
   for i=1, info.nups do
      debug.upvaluejoin(copy, i, self, i)
   end
   setfenv(copy, getfenv(self))
   return copy
end
function Function.__proto.compose(f1, f2)
   return function(...)
      return f1(f2(...))
   end
end
function Function.__proto.andthen(f1, f2)
   return f2:compose(f1)
end
function Function.__uadd(f1, f2)
   return f2:compose(f1)
end
debug.setmetatable(function() end, Function)

local Module = setmetatable({ __name = 'Module' }, Meta)
function Module.__index(self, k)
   if self.__proto[k] then
      return self.__proto[k]
   end
   return nil
end
function Module.__tostring(self)
   if self.toString then
      return self:toString()
   else
      return string.format('%s', self.__name)
   end
end
function Module.__call(self, ...)
   if self.__apply then
      return self:__apply(...)
   end
   local body = self.__body:clone()
   local name = self.__name .. '@' .. string.format('%p', module)
   local module = { __body = body, __name = name }
   module.__proto = { }
   module.__mixin = { }

   module.__is = function(self, that)
      if that == self then return true end
      local m = getmetatable(that)
      if m == Module then m = that end
      if m and m.__mixin and m.__mixin[self] then
         return true
      end
      return false
   end

   setfenv(body, setmetatable({ __self__ = module }, { __index = getfenv(2) }))
   body(setmetatable(module, Module), ...)
   return module
end

local function module(name, body)
   local module = { __name = name, __body = body }
   module.__proto = { }
   module.__mixin = { }

   module.__is = function(self, that)
      if that == self then return true end
      local m = getmetatable(that)
      if m == Module then m = that end
      if m and m.__mixin and m.__mixin[self] then
         return true
      end
      return false
   end

   setfenv(body, setmetatable({ __self__ = module }, { __index = getfenv(2) }))

   body(setmetatable(module, Module))

   return module
end

Class = setmetatable({ __name = 'Class' }, Meta)
function Class.__call(class, ...)
   local obj
   if class.__apply then
      obj = class:__apply(...)
   else
      obj = { }
      setmetatable(obj, class)
      if class.__proto.self then
         class.__proto.self(obj, ...)
      end
   end
   return obj
end
function Class.__tostring(class)
   return string.format("%s", class.__name)
end

Object = setmetatable({ }, Class)
Object.__name = 'Object'
Object.__body = function(self) end
Object.__proto = { }
Object.__mixin = { }

local special = {
   __add__ = { '__add', function(a, b) return a:__add__(b) end };
   __sub__ = { '__sub', function(a, b) return a:__sub__(b) end };
   __mul__ = { '__mul', function(a, b) return a:__mul__(b) end };
   __div__ = { '__div', function(a, b) return a:__div__(b) end };
   __pow__ = { '__pow', function(a, b) return a:__pow__(b) end };
   __mod__ = { '__mod', function(a, b) return a:__mod__(b) end };
   __len__ = { '__len', function(a, b) return a:__len__(b) end };
   __unm__ = { '__unm', function(a, b) return a:__unm__(b) end };
   __get__ = { '__getindex',  function(a, k) return a:__get__(k) end };
   __set__ = { '__setindex',  function(a, k, v) a:__set__(k, v) end };
   __concat__ = { '__concat', function(a, b) return a:__concat__(b) end };
   __pairs__  = { '__pairs',  function(a, b) return a:__pairs__() end };
   __ipairs__ = { '__ipairs', function(a, b) return a:__ipairs__() end };
   __call__   = { '__call',   function(self, ...) return self:__call__(...) end };
   __each__   = { '__each',   function(self) return self:__each__() end };
   toString   = { '__tostring', function(self, ...) return self:toString(...) end };
}

local function stringify(o)
   local m = getmetatable(o)
   local b = { }
   -- XXX: this wants a reflection API
   for k,v in pairs(o) do
      if k ~= '__info' then
         b[#b + 1] = tostring(k) .. "=" .. tostring(v)
      end
   end
   return tostring(m) .. "(" .. table.concat(b, ",") .. ")"
end

local function class(name, body, ...)
   local base
   if select('#', ...) > 0 then
      if select(1, ...) == nil then
         error("attempt to extend a 'nil' value", 2)
      end
      base = ...
   end

   if not base then base = Object end

   local class = { __name = name, __base = base, __body = body }
   local __proto = { }
   local __mixin = { }

   setmetatable(__proto, { __index = base.__proto })
   setmetatable(__mixin, { __index = base.__mixin })

   class.__proto = __proto
   class.__mixin = __mixin

   if getmetatable(base) == Module then
      class.__mixin[base] = true
   end

   function class.__index(o, k)
      if __proto[k] then
         return __proto[k]
      end
      if __proto.methodMissing then
         return __proto.methodMissing(o, k)
      end
      return nil
   end
   function __proto.toString(o)
      -- return string.format('<%s>: %p', tostring(class.__name), o)
      return stringify(o)
   end

   body(setmetatable(class, Class), base.__proto)

   for name, delg in pairs(special) do
      if __proto[name] then
         class[delg[1]] = delg[2]
      end
   end
   if class.__finalize then
      local retv = class:__finalize()
      if retv ~= nil then
         return retv
      end
   end
   return class
end

function include(into, ...)
   for i=1, select('#', ...) do
      if select(i, ...) == nil then
         error("attempt to include a nil value", 2)
      end
   end

   local args = { ... }
   for i=1, #args do
      local from = args[i]
      for k,v in pairs(from.__proto) do
         into.__proto[k] = v
      end
      if from.__included then
         from:__included(into)
      end
      if from.__mixin then
         for k,v in pairs(from.__mixin) do
            into.__mixin[k] = true
         end
      end
      into.__mixin[from] = true
   end
end

local function is_classy(m)
   return m == Module or m == Class or m == Grammar
end
local function with(this, that)
   local this_m, that_m = getmetatable(this), getmetatable(that)
   if is_classy(this_m) and is_classy(that_m) then
      local n = tostring(this).."+"..tostring(that)
      local m = module(n, function(self)
         include(self, this, that)
      end)
      return m
   elseif is_classy(that_m) then
      local n = string.format("%s: %p", typeof(this), this).."+"..tostring(that)
      local m = module(n, function(self)
         include(self, that)
         for k,v in pairs(this) do
            self[k] = v
         end
      end)
      return m
   elseif is_classy(this_m) then
      local n = tostring(this).."+"..string.format("%s: %p", typeof(that), that)
      local m = module(n, function(self)
         include(self, this)
         for k,v in pairs(that) do
            self[k] = v
         end
      end)
      return m
   else
      local n = string.format("%s: %p", typeof(this), this).."+"..string.format("%s: %p", typeof(that), that)
      local m = module(n, function(self)
         self.__mixin[this_m] = true
         self.__mixin[that_m] = true
         for k,v in pairs(this) do self[k] = v end
         for k,v in pairs(that) do self[k] = v end
      end)
      return m
   end

end


local Array = class("Array", function(self)
   local Array = self
   local unpack, select, table = unpack, select, table

   function self:__apply(...)
      return setmetatable({
         length = select('#', ...), [0] = select(1, ...), select(2, ...)
      }, self)
   end
   function self.__each(a)
      local l = a.length
      local i = -1
      return function(a)
         i = i + 1
         local v = a[i]
         if i < l then
            return i, v
         end
         return nil
      end, a
   end
   function self.__pairs(self)
      return function(self, ctrl)
         local i = ctrl + 1
         if i < self.length then
            return i, self[i]
         end
      end, self, -1
   end
   function self.__ipairs(self)
      return function(self, ctrl)
         local i = ctrl + 1
         if i < self.length then
            return i, self[i]
         end
      end, self, -1
   end

   function self.__proto:join(sep)
      local t = { }
      for i=0, #self - 1 do
         t[#t + 1] = tostring(self[i])
      end
      return table.concat(t, sep)
   end
   function self.__proto:push(val)
      self[self.length] = val
   end
   function self.__proto:pop()
      local last = self[self.length - 1]
      self[self.length - 1] = nil
      self.length = self.length - 1
      return last
   end
   function self.__proto:shift()
      local v = self[0]
      local l = self.length
      for i=1, l - 1 do
         self[i - 1] = self[i]
      end
      self.length = l - 1
      self[l - 1] = nil
      return v
   end
   function self.__proto:unshift(v)
      local l = self.length
      for i = l - 1, 0, -1 do
         self[i + 1] = self[i]
      end
      self.length = l + 1
      self[0] = v
   end
   function self.__proto:slice(offset, count)
      local a = Array()
      for i=offset, offset + count - 1 do
         a[a.length] = self[i]
      end
      return a
   end
   function self.__proto:reverse()
      local a = Array()
      for i = self.length - 1, 0, -1 do
         a[a.length] = self[i]
      end
      return a
   end
   local gaps = {
      1391376, 463792, 198768, 86961, 33936, 13776,
      4592, 1968, 861, 336, 112, 48, 21, 7, 3, 1
   }
   local less = function(a, b) return a < b end
   function self.__proto:sort(cmp, n)
      n = n or self.length
      cmp = cmp or less
      for i=1, #gaps do
         local gap = gaps[i]
         for i = gap, n - 1 do
           local v = self[i]
           for j = i - gap, 0, -gap do
             local tv = self[j]
             if not cmp(v, tv) then break end
             self[i] = tv
             i = j
           end
           self[i] = v
         end
       end
       return self
   end
   function self.__spread(a)
      return unpack(a, 0, a.length - 1)
   end
   function self.__len(a)
      return a.length
   end
   function self.__tostring(a)
      if a.toString then
         return a:toString()
      end
      return string.format("<Array>: %p", self)
   end
   function self.__index(a, k)
      if Array.__proto[k] then
         return Array.__proto[k]
      end
      if type(k) == 'number' and k < 0 then
         return a[#a + k]
      end
      if type(k) == 'table' and getmetatable(k) == Range then
         local l, r = k.left, k.right
         if l < 0 then l = a.length + l end
         if r < 0 then r = a.length + r end
         return Array.__proto.slice(a, l, r - l)
      end
      return nil
   end
   function self.__newindex(a, k, v)
      if type(k) == 'number' and k >= a.length then
         a.length = k + 1
      end
      rawset(a, k, v)
   end
   function self.__proto:toString()
      local b = { }
      for i=0, self.length - 1 do
         b[#b + 1] = tostring(self[i])
      end
      return '['..table.concat(b, ',')..']'
   end
   function self.__proto:map(f)
      local b = Array()
      for i=0, self.length - 1 do
         b[i] = f(self[i])
      end
      return b
   end
   function self:__match(that)
      if type(that) == "table" then
         if not __is__(that, Array) then
            return false
         end
         if that.length ~= self.length then
            return false
         end
         for i=0, self.length - 1 do
            if not __match__(self[i], that[i]) then
               return false
            end
         end
         return true
      end
   end
end)

local Any = class("Any", function(self)
   function self.__match(self, that)
      for i=1, #self do
         if __match__(self[i], that) then
            return true
         end
      end
      return false
   end
end)

local function any(...)
   return setmetatable({ ... }, Any)
end

local All = class("All", function(self)
   function self.__match(self, that)
      for i=1, #self do
         if not __match__(self[i], that) then
            return false
         end
      end
      return true
   end
end)

local function all(...)
   return setmetatable({ ... }, All)
end

local function try(try, catch, finally)
   local ok, rv = pcall(try)
   if not ok and catch ~= nil then
      ok, rv = pcall(catch, rv)
      if not ok then
         error("error in error handling", 2)
      end
   end
   if finally then finally() end
   return rv
end

local String = class("String", function(self)
   local string = _G.string
   for k, v in pairs(string) do
      self.__proto[k] = v
   end
   self.__apply = function(_, v)
      return tostring(v)
   end
   self.__getindex = function(o, k)
      local t = type(k)
      if t == "table" and getmetatable(k) == Range then
         return string.sub(o, k.left, k.right)
      elseif t == 'number' then
         return string.sub(o, k, k)
      end
   end
   self.__proto.self = function(self, that)
      return tostring(that)
   end
   self.__proto.split = function(self, sep, max, raw)
      if not max then
         max = math.huge
      end
      if not sep then
         sep = '%s+'
      end
      local out = { }
      local pos = 1
      while max > 1 do
         local lhs, rhs = string.find(self, sep, pos, raw)
         if not lhs then
            break
         end
         if sep == "" then
            out[#out + 1] = string.sub(self, pos, lhs)
            pos = lhs + 1
         else
            out[#out + 1] = string.sub(self, pos, lhs - 1)
            pos = rhs + 1
         end
         max = max - 1
      end
      out[#out + 1] = string.sub(self, pos)
      return out
   end
   self.__proto.toString = tostring
end)
debug.setmetatable("", String)

local Error = class("Error", function(self)
   self.__proto.self = function(self, mesg)
      self.message = mesg
      self.trace = debug.traceback(mesg, 2)
   end
   self.__proto.toString = function(self)
      return self.message
   end
end)

local function spread(o)
   local m = getmetatable(o)
   if m and m.__spread then
      return m.__spread(o)
   end
   return unpack(o)
end
local function each(o, ...)
   if type(o) == 'function' then
      return o, ...
   end
   local m = getmetatable(o)
   if m and m.__each then
      return m.__each(o, ...)
   end
   return pairs(o)
end

Range = { }
Range.__index = Range
function Range.__match(self, that)
   local n = tonumber(that)
   if type(n) == 'number' and n == n then
      return n >= self.left and n <= self.right
   end
   return false
end
function Range.__tostring(self)
   return string.format("Range(%s, %s)", self.left, self.right)
end
function Range.__each(self)
   local i, r = self.left, self.right
   local n = i <= r and 1 or -1
   return function()
      local j = i
      i = i + n
      if n > 0 and j > r then
         return nil
      elseif n < 0 and j < r then
         return nil
      end
      return j
   end
end

local function range(left, right, incl)
   return setmetatable({
      left  = left,
      right = right,
      incl  = incl,
   }, Range)
end

local function import(path, ...)
   local from = path
   if type(from) == 'string' then
      from = require(from)
   end
   local list = { }
   for i=1, select('#', ...) do
      local key = select(i, ...)
      local val = from[key]
      if val == nil then
	 local pkg
	 if type(path) == 'string' then
	    pkg = string.format("%q", path)
	 else
	    pkg = tostring(path)
	 end
	 error(string.format("import %q from %s is nil", key, pkg), 2)
      end
      list[i] = val
   end
   return unpack(list)
end

local ArrayPattern, TablePattern, ApplyPattern

local __var__ = newproxy()

function __match__(that, this)
   local type_this = type(this)
   local type_that = type(that)

   local meta_that = getmetatable(that)
   if meta_that then
      if meta_that.__match then
         return meta_that.__match(that, this)
      elseif __is__(this, that) then
         return true
      else
         return this == that
      end
   elseif type_this ~= type_that then
      return false
   else
      return this == that
   end
end

local function expand(iter, stat, ctrl, ...)
   if iter == nil then return ... end
   local k, v, _1, _2, _3 = iter(stat, ctrl)
   if k == nil then return ... end
   if v == __var__ then
      return expand(_1, _2, _3, expand(iter, stat, k, ...))
   end
   return v, expand(iter, stat, k, ...)
end

local function extract(patt, subj)
   return expand(patt:bind(subj))
end

TablePattern = class("TablePattern", function(self)
   self.__apply = function(self, keys, desc, meta)
      return setmetatable({
         keys = keys;
         desc = desc;
         meta = meta;
      }, self)
   end

   self.__pairs = function(self)
      local i = 0
      return function(self, _)
         i = i + 1
         local k = self.keys[i]
         if k ~= nil then
            return k, self.desc[k]
         end
      end, self, nil
   end

   self.__match = function(self, that)
      if type(that) ~= 'table' then
         return false
      end
      local desc = self.desc
      local meta = self.meta
      if meta and getmetatable(that) ~= meta then
         return false
      end
      for k, v in pairs(self) do
         if v == __var__ then
            if that[k] == nil then
               return false
            end
         else
            if not __match__(v, that[k]) then
               return false
            end
         end
      end
      return true
   end

   self.__proto.bind = function(self, subj)
      if subj == nil then return end
      local meta = self.meta
      local iter, stat, ctrl = pairs(self)
      return function(stat, ctrl)
         for k, v in iter, stat, ctrl do
            if v == __var__ then
               if meta then
                  -- XXX: assert instead?
                  return k, meta.__index(subj, k)
               else
                  return k, subj[k]
               end
            elseif type(v) == 'table' then
               return k, __var__, v:bind(subj[k])
            end
         end
      end, stat, ctrl
   end
end)

ArrayPattern = class("ArrayPattern", function(self)
   self.__apply = function(self, ...)
      return setmetatable({
         length = select('#', ...), [0] = select(1, ...), select(2, ...)
      }, self)
   end

   self.__ipairs = function(self)
      return function(self, ctrl)
         local i = ctrl + 1
         if i < self.length then
            return i, self[i]
         end
      end, self, -1
   end

   self.__match = function(self, that)
      if type(that) ~= 'table' then
         return false
      end
      if getmetatable(that) ~= Array then
         return false
      end
      for i, v in ipairs(self) do
         if v ~= __var__ then
            if not __match__(v, that[i]) then
               return false
            end
         end
      end
      return true
   end

   self.__proto.bind = function(self, subj)
      if subj == nil then return end
      local iter, stat, ctrl = ipairs(self)
      return function(stat, ctrl)
         for i, v in iter, stat, ctrl do
            if v == __var__ then
               return i, subj[i]
            elseif type(v) == 'table' then
               return i, __var__, v:bind(subj[i])
            end
         end
      end, stat, ctrl
   end

end)

ApplyPattern = class("ApplyPattern", function(self)
   self.__apply = function(self, base, ...)
      return setmetatable({
         base = base,
         narg = select('#', ...),
         ...
      }, self)
   end

   self.__match = function(self, that)
      local base = self.base
      if base.__match then
         return base.__match(base, that)
      end
      return getmetatable(that) == self.base
   end

   self.__proto.bind = function(self, subj)
      if subj == nil then return end
      local i = 1
      local si, ss, sc
      if self.base.__unapply then
         si, ss, sc = self.base:__unapply(subj)
      elseif type(subj) == 'table' then
         si, ss, sc = ipairs(subj)
      else
         error("cannot bind "..tostring(subj).." to: "..tostring(self.base))
      end
      local last = false
      return function(self)
         while i <= self.narg do
            local k = i
            local v = self[i]
            i = i + 1
            if last then
               return k, nil
            end
            local _k, _v = si(ss, sc)
            if _k == nil then
               last = true
            end
            sc = _k
            if v == __var__ then
               return k, _v
            elseif type(v) == 'table' then
               return k, __var__, v:bind(_v)
            end
         end
      end, self, nil
   end
end)

local Pattern = setmetatable(getmetatable(lpeg.P(1)), Meta)
Pattern.__name = 'Pattern'
Pattern.__call = function(self, ...)
   return self:match(...)
end
Pattern.__tostring = function(self)
   return string.format('Pattern<%p>', self)
end
Pattern.__match = function(self, subj)
   if type(subj) ~= 'string' then return false end
   return self:match(subj)
end
Pattern.__index.__match = function(self, subj, ...)
   if type(subj) ~= 'string' then return false end
   return self:match(subj, ...)
end
Pattern.__index.__unapply = function(self, subj)
   return ipairs{ self:match(subj) }
end
Pattern.__index.find = function(self, subj, i)
  local patt = self / 0
  patt = lpeg.P{ lpeg.Cp() * patt * lpeg.Cp() + 1 * lpeg.V(1) }
  local i, e = patt:match(subj, i or 1)
  if i then
     return i, e - 1
  else
     return i
  end
end
Pattern.__index.gsub = function(self, subj, rep)
  local patt = lpeg.Cs((self / rep + 1)^0)
  return patt:match(subj)
end


local Grammar = setmetatable({ __name = 'Grammar' }, Meta)

function Grammar.__tostring(self)
   return string.format("Grammar<%s>", self.__name)
end
function Grammar.__index(self, k)
   if self.__proto[k] then
      return self.__proto[k]
   end
   return nil
end
function Grammar.__call(self, subj, ...)
   return self:__match(subj, ...)
end

local function grammar(name, body, base)
   local __proto = { }
   local __mixin = { }

   if not base then base = Object end

   local gram = setmetatable({
      __name  = name,
      __body  = body,
      __base  = base,
      __proto = __proto,
      __mixin = __mixin
   }, Grammar)

   setfenv(body, setmetatable({ }, { __index = getfenv(2) }))

   include(gram, base)

   body(gram, base.__proto)

   local patt = { }
   for k, v in pairs(__proto) do
      if lpeg.type(v) == 'pattern' then
         patt[k] = v
      end
   end

   patt[1] = gram[1]

   gram.__unapply = function(self, subj)
      if not self.__patt then
         self.__patt = lpeg.P(patt)
      end
      return self.__patt:__unapply(subj)
   end
   gram.__match = function(self, subj, ...)
      if not self.__patt then
         self.__patt = lpeg.P(patt)
      end
      return self.__patt:__match(subj, ...)
   end

   return gram
end

local rule = { }
lpeg.setmaxstack(1024)
do
   local def = { }

   def.nl  = lpeg.P("\n")
   def.pos = lpeg.Cp()

   local any=lpeg.P(1)
   lpeg.locale(def)

   def.a = def.alpha
   def.c = def.cntrl
   def.d = def.digit
   def.g = def.graph
   def.l = def.lower
   def.p = def.punct
   def.s = def.space
   def.u = def.upper
   def.w = def.alnum
   def.x = def.xdigit
   def.A = any - def.a
   def.C = any - def.c
   def.D = any - def.d
   def.G = any - def.g
   def.L = any - def.l
   def.P = any - def.p
   def.S = any - def.s
   def.U = any - def.u
   def.W = any - def.w
   def.X = any - def.x

   rule.def = def
   rule.Def = function(id)
      if def[id] == nil then
         error("No predefined pattern '"..tostring(id).."'", 2)
      end
      return def[id]
   end

   local mm = getmetatable(lpeg.P(0))
   mm.__mod = mm.__div

   rule.__add = mm.__add
   rule.__sub = mm.__sub
   rule.__pow = mm.__pow
   rule.__mul = mm.__mul
   rule.__div = mm.__div
   rule.__len = mm.__len
   rule.__unm = mm.__unm
   rule.__mod = mm.__div

   for k,v in pairs(lpeg) do rule[k] = v end

   local function backref(s, i, c)
      if type(c) ~= "string" then return nil end
      local e = #c + i
      if string.sub(s, i, e - 1) == c then
         return e
      else
         return nil
      end
   end

   rule.Cbr = function(name)
      return lpeg.Cmt(lpeg.Cb(name), backref)
   end
end

local __magic__
local function environ(mod)
   return setmetatable(mod, { __index = __magic__ })
end
local function warn(msg, lvl)
   local info = debug.getinfo((lvl or 1) + 1, "Sl")
   local tmpl = "%s:%s: %s\n"
   io.stderr:write(tmpl:format(info.short_src, info.currentline, msg))
end

local bit = require("bit")

local Nil       = setmetatable({ __name = 'Nil'       }, Meta)
local Number    = setmetatable({ __name = 'Number'    }, Meta)
local Boolean   = setmetatable({ __name = 'Boolean'   }, Meta)
local Table     = setmetatable({ __name = 'Table'     }, Meta)
local UserData  = setmetatable({ __name = 'UserData'  }, Meta)
local Coroutine = setmetatable({ __name = 'Coroutine' }, Meta)
local CData     = setmetatable({ __name = 'CData'     }, Meta)

Table.__proto = { }
Table.__index = Table.__proto
function Table.__apply(self, proto)
   return setmetatable({ proto }, self)
end
function Table.__proto:__getindex(key)
   return self[1][key]
end
function Table.__proto:__setindex(key, val)
   self[1][key] = val
end
function Table.__each(self)
   return pairs(self[1])
end

for k, v in pairs(table) do
   Table[k] = v
end
function Table.keys(t)
   local ks = { }
   for k, v in pairs(t) do
      ks[#ks + 1] = k
   end
   return ks
end

for k, v in pairs(coroutine) do
   Coroutine[k] = v
end
for k, v in pairs(ffi) do
   CData[k] = v
end

local native = {
   [Nil]       = 'nil',
   [Number]    = 'number',
   [Boolean]   = 'boolean',
   [String]    = 'string',
   [Table]     = 'table',
   [Function]  = 'function',
   [Coroutine] = 'thread',
   [UserData]  = 'userdata',
   [CData]     = 'cdata',
}

local function _is_type(m_a, m_b)
   if m_a == nil then
      return false
   elseif m_a == m_b then
      return true
   else
      return _is_type(m_a.__base, m_b)
   end
end

function __is__(a, b)
   if b == Dynamic then
      return true
   end
   if type(b) == 'table' and b.__is then
      return b:__is(a)
   end
   if type(a) == 'cdata' then
      return ffi.istype(b, a)
   elseif getmetatable(a) == b then
      return true
   elseif native[b] then
      return type(a) == native[b]
   elseif b == Pattern then
      return lpeg.type(a) == 'pattern'
   elseif getmetatable(b) == Class then
      return _is_type(getmetatable(a), b)
   end
   return false
end

local typemap = { }
for k, v in pairs(native) do
   typemap[v] = k
end
local function typeof(a)
   local t = type(a)
   if t == 'table' or t == 'userdata' then
      local m = getmetatable(a)
      if m then return m end
   end
   if t == 'cdata' then
      return ffi.typeof(a)
   end
   return typemap[type(a)]
end

local function check(name, expr, type)
   if not __is__(expr, type) then
      error(string.format("bad assignment to '%s' (%s expected got %s)",
         name,
         tostring(type),
         tostring(typeof(expr))
      ), 2)
   end
   return expr
end

local usrop_events = {
   [':!'] = '__ubang',
   [':?'] = '__uques',
   [':='] = '__ueq',
   [':>'] = '__ugt',
   [':<'] = '__ult',
   [':|'] = '__upipe',
   [':^'] = '__ucar',
   [':&'] = '__uamp',
   [':~'] = '__utilde',
   [':+'] = '__uadd',
   [':-'] = '__usub',
   [':*'] = '__umul',
   [':/'] = '__udiv',
   [':%'] = '__umod'
}

local function usrop(op, a, b)
   local o, mt, h = usrop_events[op]
   mt = getmetatable(a)
   if mt and mt[o] then
      h = mt[o]
   end
   if h == nil then
      mt = getmetatable(b)
      if mt and mt[o] then
         h = mt[o]
      end
      if h == nil then
         error(string.format("no handler for operator '%s'", op), 2)
      end
   end
   return h(a, b)
end

local function __in__(names, expr)
   local t = { }
   for i=1, #names do
      t[i] = expr[names[i]]
   end
   return unpack(t, 1, #names)
end

local function traceback(lvl)
   local buf = { }
   while true do
      local info = debug.getinfo(lvl, 'nlS')
      if info then
         local name = info.name
         local what = info.namewhat
         local file = info.short_src
         local line = info.currentline
         local ltop = info.linedefined
         local lbot = info.lastlinedefined
         local map  = __magic__._MAPS[file]
         if map then
            name = map[name] or name or '<main>'
            line = map[line]
            ltop = map[ltop]
            lbot = map[lbot]
            if ltop then
               buf[#buf + 1] = string.format(
                  "%s:%s: [%s:%s-%s] %s",
                  file, line, what, ltop, lbot, name
               )
            else
               buf[#buf + 1] = string.format(
                  "%s:%s: %s", file, line, name
               )
            end
         end
      else
         break
      end
      lvl = lvl + 1
   end
   return "\t"..table.concat(buf, "\n\t")
end

local patterns = {
   "^(.-):(%d+): ([^']+)'([^']+)'(.-)$",
   "^(.-):(%d+): (.-)$"
}
local formats = {
   "%s:%s: %s'%s'%s",
   "%s:%s: %s"
}
local function parse_error(msg)
   for i=1, #patterns  do
      local _1, _2, _3, _4, _5 = msg:match(patterns[i])
      if _1 then
         return i, _1, _2, _3, _4, _5
      end
   end
end

local function errfunc(e)
   if type(e) == "string" then
      local fidx, file, line, mesg, name, xtra = parse_error(e)
      if fidx then
         local map = __magic__._MAPS[file]
         if map then
            local fmt = formats[fidx]
            local i = map[tonumber(line)]
            local f, l
            if type(i) == 'table' then
               f, l = i[1], i[2]
            else
               f, l = file, i or line
            end
            local m = mesg
            local n = map[name] or name
            local x = xtra
            local m = string.format(fmt,f,l,m,n,x)
            local s = m .. "\n" .. traceback(2)
            print(s)
            return s
         else
            local s = e .. "\n" .. debug.traceback()
            print(s)
            return s
         end
      else
         print(e)
         return e
      end
   end
end

local function run(path, ...)
   local unit = require(path)
   if not rawget(unit, 'main') then
      error("no main defined in '"..path..'"')
   end
   xpcall(unit.main, errfunc, ...)
end

local function new(base, info, ...)
   local inst = setmetatable({ }, base)
   if info then
      inst.__info = info
   end
   if base.__proto.self then
      base.__proto.self(inst, ...)
   end
   return inst
end

__magic__ = {
   _MAPS = { };

   -- builtin types
   Meta = Meta;
   Dynamic = Dynamic;
   Nil = Nil;
   Number = Number;
   Boolean = Boolean;
   String = String;
   Function = Function;
   Coroutine = Coroutine;
   UserData = UserData;
   Table = Table;
   Array = Array;
   Error = Error;
   Module = Module;
   Grammar = Grammar;
   Class = Class;
   Object = Object;
   Pattern = Pattern;
   ArrayPattern = ArrayPattern;
   TablePattern = TablePattern;
   ApplyPattern = ApplyPattern;

   -- builtin functions
   try = try;
   class = class;
   module = module;
   import = import;
   yield = coroutine.yield;
   throw = error;
   warn = warn;
   grammar = grammar;
   include = include;
   typeof = typeof;
   eval = eval;
   any = any;
   all = all;

   -- utility
   environ = environ;

   -- constants
   null = null;

   __usrop__ = usrop;

   -- operators
   __with__ = with;
   __check__ = check;
   __rule__ = rule;
   __range__ = range;
   __spread__ = spread;
   __match__ = __match__;
   __extract__ = extract;
   __each__ = each;
   __var__ = __var__;
   __in__ = __in__;
   __is__ = __is__;
   __as__ = setmetatable;
   __lshift__ = bit.lshift;
   __rshift__ = bit.rshift;
   __arshift__ = bit.arshift;
   __bnot__ = bit.bnot;
   __band__ = bit.band;
   __bor__ = bit.bor;
   __bxor__ = bit.bxor;
   __take__ = coroutine.yield;
   __set_type_info__ = set_type_info;
   run = run;
   new = new;

   _VERSION = "0.0.1";
}

setmetatable(__magic__, {
   __index = function(t, n)
      local v = rawget(_G, n)
      if v == nil then
         error("variable '"..n.."' is not declared", 2)
      end
      rawset(t, n, v)
      return v
   end
})

table.insert(package.loaders, 1, function(path)
   if path:sub(-4) == '.blz' then
      local body = package.preload[path]
      return function(...)
         local _, exports = xpcall(body, errfunc, ...)
         return exports
      end
   end
end)
package.loaded["blaze.core"] = __magic__
return __magic__

