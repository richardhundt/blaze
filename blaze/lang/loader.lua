--local tvm        = require('tvm')
local compiler = require('blaze.lang.compiler')

local magic = string.char(0x1b, 0x4c, 0x4a, 0x01)

local function loadchunk(code, name, opts)
   if string.sub(code, 1, #magic) ~= magic then
      code = compiler.compile(code, name, opts)
   end
   return loadstring(code, '@'..name)
end

local function loadfile(filename)
   local file = io.open(filename)
   if file then
      local code = file:read('*a')
      file:close()
      if string.sub(filename, -4) == '.blz' then
         return assert(loadchunk(code, filename, opts))
      else
         return assert(loadstring(code, '@'..filename))
      end
   else
      -- die?
   end
end

local function loader(modname, opts)
   local filename, havepath
   if string.find(modname, '/') or string.sub(modname, -4) == '.blz' then
      filename = modname
   else
      filename = package.searchpath(modname, package.path)
   end
   if filename then
      return loadfile(filename)
   end
end

return {
   loader = loader;
   loadfile = loadfile;
   loadchunk = loadchunk;
}

