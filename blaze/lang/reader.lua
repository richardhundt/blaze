local Process = require("blaze.lang.process")

local Reader = { } do
   Reader.__index = Reader

   local yield = coroutine.yield
   local remove = table.remove

   function Reader.new(ctx)
      return setmetatable({
         ctx   = ctx;
         queue = { };
      }, Reader)
   end

   function Reader:spawn()
      return Process.new(function()
         while #self.queue > 0 do
            local unit = remove(self.queue, 1)
            if unit.from then
               self.ctx:set_file(unit.from.path)
               self.ctx:set_line(unit.from.line)
            end
            unit.text = self:read(unit.path)
            yield(unit)
         end
      end)
   end

   function Reader:enqueue(item)
      self.queue[#self.queue + 1] = item
   end

   function Reader:read(path)
      local file, err = io.open(path)
      if not file then
         self.ctx:abort(err)
      end
      local src = file:read"*a"
      file:close()
      return src
   end
end

return Reader

