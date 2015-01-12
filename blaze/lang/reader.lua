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
            local item = remove(self.queue, 1)
            if item.from then
               self.ctx:set_file(item.from.file)
               self.ctx:set_line(item.from.line)
            end
            item.text = self:read(item.path)
            yield(item)
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

