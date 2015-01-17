local model   = require("blaze.lang.model")
local Reader  = require("blaze.lang.reader")
local Parser  = require("blaze.lang.parser")
local Definer = require("blaze.lang.definer")
local Checker = require("blaze.lang.checker")
local Emitter = require("blaze.lang.emitter")

local Context = { } do
   Context.__index = Context

   function Context.new()
      local self = setmetatable({
         file = "";
         line = 0;
      }, Context)

      self.universe = model.Universe.new()
      self.registry = model.Registry.new()

      self.reader  = Reader.new(self)
      self.parser  = Parser.new(self)
      self.definer = Definer.new(self)
      self.checker = Checker.new(self)
      self.emitter = Emitter.new(self)

      return self
   end

   function Context:set_line(line)
      self.line = line
   end
   function Context:get_line()
      return self.line
   end
   function Context:sync_line(line)
      if line then
         self.line = line
      end
      return self.line
   end

   function Context:set_file(file)
      self.file = file
   end
   function Context:get_file()
      return self.file
   end

   function Context:abort(mesg)
      mesg = string.format(
         "blaze: %s:%s: %s\n", self.file, self.line, mesg
      )
      io.stderr:write(mesg)
      os.exit(1)
   end

end

local Compiler = { } do

   Compiler.__index = Compiler

   function Compiler.new(ctx)
      return setmetatable({ }, self)
   end

   function Compiler:compile(path)
      local ctx = Context.new(path)
      local out =
         ctx.reader:spawn()
            :pipe(ctx.parser:spawn())
            :pipe(ctx.definer:spawn())
            :pipe(ctx.checker:spawn())
            :pipe(ctx.emitter:spawn())

      local unit = model.Unit.new(path)
      ctx.reader:enqueue(unit)
      return out:run()
   end
end

return Compiler

