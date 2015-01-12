--[=[
Copyright (C) 2013-2014 Richard Hundt and contributors.
See Copyright Notice in blaze
]=]

local export = { }

local function dump(node, level, seen)
   if not seen then seen = { } end
   if not level then level = 1 end
   if type(node) == 'nil' then
      return "null"
   end
   if type(node) == "string" then
      return '"'..node..'"'
   end
   if type(node) == "number" then
      return node
   end
   if type(node) == "boolean" then
      return tostring(node)
   end
   if type(node) == "function" then
      return tostring(node)
   end
   if type(node) == 'cdata' then
      return tostring(node)
   end
   if seen[node] then
      return "@"..seen[node]
   end

   seen[node] = #seen + 1
   seen[#seen + 1] = node

   local buff = { }
   local dent = string.rep("    ", level)
   local tput = table.insert

   tput(buff, "{ #"..seen[node])
   local i_buff = { }
   local p_buff = { }
   for k,data in pairs(node) do
      tput(buff, "\n"..dent..dump(k,nil,seen)..': '..dump(data, level + 1, seen))
      if next(node, k) then
         tput(buff, ",")
      end
   end
   tput(buff, "\n"..string.rep("    ", level - 1).."}")

   return table.concat(buff, "")
end

export.dump = dump

local ID = 0
export.genid = function(prefix)
   ID = ID + 1
   prefix = prefix or '$#'
   return prefix..ID
end

function export.quote(str)
   --return (string.format("%q", str):gsub("\\\n", "\n"))
   return (string.format("%q", str):gsub("\\\n", "\\n"))
end
function export.unquote(str)
   if string.sub(str, 1, 1) == '"' and string.sub(str, -1) == '"' then
      return string.sub(str, 2, -2)
   end
   return str
end

function export.extend(base, with)
   with.__super = base
   with.__index = with
   return setmetatable(with, { __index = base, __call = base.__call })
end

function export.fold_left(list, func)
   local accu = list[1]
   for i=2, #list do
      accu = func(accu, list[i])
   end
   return accu
end

function export.fold_right(list, func)
   local accu = list[#list]
   for i=#list - 1, 1, -1 do
      accu = func(accu, list[i])
   end
   return accu
end

function export.mangle(name)
   return name:gsub('[^%w_]', function(c)
      return '_'..string.format("%.2x",string.byte(c))
   end)
end
function export.demangle(name)
   return name:gsub('_(%x%x)', function(c)
      return string.char(tonumber(c, 16))
   end)
end
function export.mixin(this, that)
   for k,v in pairs(that) do
      thus[k] = v
   end
end
function export.class(this, base, ...)
   this.__index = setmetatable(this, base)
   for i=1, select('#', ...) do
      export.mixin(this, select(i, ...))
   end
end
return export
