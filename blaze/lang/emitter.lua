local util = require("blaze.lang.util")

local Process = require("blaze.lang.process")

local Emitter = { } do
   Emitter.__index = Emitter

   local yield = coroutine.yield
   local emit = { }

   function Emitter.new()
      return setmetatable({
         level   = 0,
         dent    = '   ',
         margin  = '',
         buffer  = { },
         srcline = 1,
         srcfile = "main",
         srcmap  = { }
      }, Emitter)
   end

   function Emitter:spawn()
      return Process.new(function(input)
         for unit in input do
            self:chunk(unit.optree)
            unit.code = self:output()
            yield(unit)
         end
      end)
   end

   function Emitter:output()
      local out = table.concat(self.buffer)
      local buf = { }
      local seen = { }
      for i=1, #self.srcmap do
         local info = self.srcmap[i]
         seen[i] = true
         buf[#buf + 1] = "{"..("%q"):format(tostring(info[1]))..","..tostring(info[2]).."}"
      end
      for k,v in pairs(self.srcmap) do
         if not seen[k] then
            buf[#buf + 1] = k.."="..string.format("%q", v)
         end
      end
      local map = table.concat(buf, ",")
      return "return select(2, xpcall(function() "..out.." end, function(e) __magic__.errfunc(e, {"..map.."}) end))"
   end

   function Emitter:indent()
      self.level  = self.level + 1
      self.margin = string.rep(self.dent, self.level)
   end
   function Emitter:undent()
      self.level  = self.level - 1
      self.margin = string.rep(self.dent, self.level)
   end
   function Emitter:writeln()
      self.buffer[#self.buffer + 1] = "\n"
      self.srcmap[#self.srcmap + 1] = { self.srcfile, self.srcline }
   end
   function Emitter:emit(str)
      self.buffer[#self.buffer + 1] = str
   end
   function Emitter:write(op)
      if type(op) == "string" then
         if emit[op] then
            return emit[op](self, op)
         else
            local id = util.mangle(op)
            if id ~= op then
               self.srcmap[id] = op
            end
            return self:emit(id)
         end
      end
      return op:write(self)
   end

   function Emitter:op(op)
      if #op == 0 then return "" end
      local tag = table.remove(op, 1)
      return emit[tag](self, op)
   end
   function Emitter:list(op, idx, sep)
      for i=(idx or 1), #op do
         self:write(op[i])
         if i < #op then
            self:emit(sep or " ")
         end
      end
   end
   function Emitter:chunk(op, idx)
      for i=(idx or 1), #op do
         self:write(op[i])
         self:emit(";")
         self:writeln()
      end
   end
   function Emitter:const(op)
      local t, op = type(op[1]), op[1]
      if t == 'table' then
         self:emit("{")
         if op[0] then
            self:emit("[0]=")
            self:write(o[0])
            self:emit(";")
         end
         for i=1, #op do
            self:write(op[i])
            self:emit(";")
         end
         for k, v in pairs(op) do
            if type(k) ~= 'number' or k < 0 or k > #op then
               self:emit("[")
               self:write(k)
               self:emit("]=")
               self:write(v)
               self:emit(";")
            end
         end
         self:emit("}")
      elseif t == 'string' then
         self:emit(util.quote(op))
         --return string.format('%q', op)
      else
         self:emit(tostring(op))
      end
   end

   local unrops = {
      ['!len'] = "#",
      ['!neg'] = "-",
      ['!not'] = "not"
   }
   local binops = {
      ['!add'] = "+",
      ['!sub'] = "-",
      ['!mul'] = "*",
      ['!div'] = "/",
      ['!mod'] = "%",
      ['!pow'] = "^",
      ['!eq'] = "==",
      ['!ne'] = "~=",
      ['!ge'] = ">=",
      ['!le'] = ">=",
      ['!gt'] = ">",
      ['!lt'] = "<"
   }

   emit['!nil'] = function(w, op)
      w:emit"nil"
   end

   emit['!true'] = function(w, op)
      w:emit"true"
   end

   emit['!false'] = function(w, op)
      w:emit"false"
   end

   emit['!vararg'] = function(w, op)
      w:emit"..."
   end

   for key, oper in pairs(unrops) do
      emit[key] = function(w, op)
         w:emit(oper.."(")
         w:write(op[1])
         w:emit(")")
      end
   end

   for key, oper in pairs(binops) do
      emit[key] = function(w, op)
         w:emit"("
         w:write(op[1])
         w:emit(")"..oper.."(")
         w:write(op[2])
         w:emit")"
      end
   end

   emit['!assign'] = function(w, op)
      return emit['!massign'](w, {{ op[1] }, { op[2] }})
   end

   emit['!massign'] = function(w, op)
      w:list(op[1], 1, ", ")
      w:emit(" = ")
      w:list(op[2], 1, ", ")
   end

   emit['!and'] = function(w, op)
      w:write(op[1])
      w:emit(" and ")
      w:write(op[2])
   end

   emit['!or'] = function(w, op)
      w:write(op[1])
      w:emit(" or ")
      w:write(op[2])
   end

   emit['!index'] = function(w, op)
      w:emit"("
      w:write(op[1])
      w:emit")["
      w:write(op[2])
      w:emit"]"
   end

   emit['!call'] = function(w, op)
      w:emit"("
      w:write(op[1])
      w:emit")("
      w:list(op, 2, ", ")
      w:emit")"
   end

   emit['!call1'] = function(w, op)
      w:emit"("
      emit['!call'](w, op)
      w:emit")"
   end

   emit['!callmeth'] = function(w, op)
      w:emit"("
      w:write(op[1])
      w:emit"):"
      w:write(op[2])
      w:emit"("
      w:list(op, 3, ", ")
      w:emit")"
   end

   emit['!callmeth1'] = function(w, op)
      w:emit"("
      emit['!callmeth'](w, op)
      w:emit")"
   end

   emit['!label'] = function(w, op)
      w:emit"::"
      w:write(op[1])
      w:emit"::"
   end

   emit['!goto'] = function(w, op)
      w:emit"goto "
      w:write(op[1])
   end

   emit['!do'] = function(w, op)
      w:emit"do "
      w:writeln()
      w:indent()
      w:chunk(op)
      w:undent()
      w:emit" end"
      w:writeln()
   end

   emit['!if'] = function(w, op)
      w:emit"if "
      w:write(op[1])
      w:emit" then "
      w:writeln()
      w:indent()
      w:write(op[2])
      w:undent()
      if op[3] then
         w:emit" else "
         w:writeln()
         w:indent()
         w:write(op[3])
         w:undent()
      end
      w:writeln()
      w:undent()
      w:emit" end"
      w:writeln()
   end

   emit['!cond'] = function(w, op)
      for i=1, #op do
         w:emit"if "
         w:write(op[i][1])
         w:emit" then "
         w:chunk(op[i][2])
         if i < #op then
            w:emit" else"
         end
      end
      w:emit" end"
   end

   emit['!break'] = function(w, op)
      w:emit"do break end"
   end

   emit['!return'] = function(w, op)
      w:emit"do return "
      w:list(op, 1, ", ")
      w:emit" end"
   end

   emit['!while'] = function(w, op)
      w:emit"while "
      w:write(op[1])
      w:emit" do "
      w:chunk(op, 2)
      w:emit" end"
   end

   emit['!repeat'] = function(w, op)
      local expr = table.remove(op)
      w:emit"repeat "
      w:chunk(op)
      w:emit" until "
      w:write(expr)
   end

   emit['!loop'] = function(w, op)
      w:emit"for "
      w:write(op[1])
      w:emit"="
      w:write(op[2])
      w:emit", "
      w:write(op[3])
      w:emit", "
      w:write(op[4])
      w:emit" do "
      w:chunk(op, 5)
      w:emit" end"
   end

   emit['!for'] = function(w, op)
      w:emit"for "
      w:list(op[1])
      w:emit" in "
      w:list(op[2], 1, ", ")
      w:emit" do "
      w:chunk(op, 3)
      w:emit" end"
   end

   emit['!define'] = function(w, op)
      if type(op[1]) ~= "table" then
         return emit['!define'](w, {{ op[1] }, { op[2] }})
      else
         if #op > 1 and #op[2] > 0 then
            w:emit"local "
            w:list(op[1])
            w:emit" = "
            w:list(op[2])
         else
            w:emit "local "
            w:list(op[1])
         end
      end
   end

   emit['!let'] = emit['!define']

   emit['!lambda'] = function(w, op)
      w:emit"function("
      w:list(op[1], 1, ", ")
      w:emit") "
      w:indent()
      w:writeln()
      w:chunk(op, 2)
      w:undent()
      w:emit" end"
      w:writeln()
   end

   emit['!concat'] = function(w, op)
      emit['!mconcat'](w, op)
   end

   emit['!mconcat'] = function(w, op)
      w:list(op, 1, "..")
   end

   emit['!line'] = function(w, op)
      local file, line
      if #op == 1 then
         line = op[1]
         file = w.srcfile
      else
         file = op[1][1]
         line = op[2]
      end
      w.srcline = line
      w.srcfile = file
      return ""
   end

end

return Emitter
