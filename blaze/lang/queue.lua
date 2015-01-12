
local Queue = { } do
   Queue.__index = Queue

   local ipairs = ipairs
   local remove = table.remove

   function Queue.new()
      return setmetatable({ }, Queue)
   end

   function Queue:ipairs()
      return ipairs(self)
   end

   function Queue:count()
      return #self
   end

   function Queue:enqueue(item)
      self[#self + 1] = item
   end

   function Queue:dequeue()
      return self:remove(1)
   end

   function Queue:remove(i)
      return remove(self, i)
   end
end

return Queue

