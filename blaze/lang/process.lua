local Process = { } do
   Process.__index = Process

   local task, yield = coroutine.wrap, coroutine.yield

   function Process.new(spawn)
      return setmetatable({
         spawn = spawn;
      }, Process)
   end

   function Process:task()
      return task(self.spawn)
   end

   function Process:run(...)
      local last
      for _ in self:task(...) do
         last = _
      end
      return last
   end

   function Process:runlog()
      local log = { }
      for _ in self:task() do
         log[#log + 1] = _
      end
      return log
   end

   function Process:pipe(that)
      return Process.new(function()
         for _ in that:task(), self:task() do
            yield(_)
         end
      end)
   end
   Process.__div = Process.pipe

   function Process:map(func)
      return Process.new(function()
         for _ in self:task() do
            yield(func(_))
         end
      end)
   end

   function Process:filter(pred)
      return Process.new(function()
         for _ in self:task() do
            if pred(_) then
               yield(_)
            end
         end
      end)
   end

   function Process:take(n)
      return Process.new(function()
         local t = self:task()
         for i=1, n do
            local v = t()
            if v == nil then
               break
            end
            yield(v)
         end
      end)
   end

   function Process:foreach(sink)
      for _ in self:task() do
         sink(_)
      end
   end
end

return Process
