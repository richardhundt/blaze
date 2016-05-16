--[=[
Copyright (C) 2013-2015 Richard Hundt and contributors.
See Copyright Notice in blaze
]=]

local getmetatable, setmetatable = getmetatable, setmetatable

local Visitor = { } do
   Visitor.__index = Visitor
   function Visitor.new(handlers)
      return setmetatable(handlers or { }, Visitor)
   end
   function Visitor:visitNode(node, ...) end
   function Visitor:visitNodeList(node, ...) end
   function Visitor:visitStatement(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitExpression(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitDeclaration(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitLiteral(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitBlockNode(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitChunkNode(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitDecoratorNode(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitIncludeStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitTraitNode(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitGrammarNode(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitClassNode(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitPropertyNode(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitMethodNode(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitParameterNode(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitParameterList(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitSignatureNode(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitFunctionNode(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitMacroNode(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitModuleStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitImportStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitImportTerm(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitExportStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitIdentifier(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitLocalDeclaration(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitInExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitRichString(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitTablePattern(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitTableItem(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitTablePair(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitTableLiteral(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitIfStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitLoop(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitWhileStatement(node, ...) return self:visitLoop(node, ...) end
   function Visitor:visitRepeatStatement(node, ...) return self:visitLoop(node, ...) end
   function Visitor:visitForStatement(node, ...) return self:visitLoop(node, ...) end
   function Visitor:visitForInStatement(node, ...) return self:visitLoop(node, ...) end
   function Visitor:visitSpreadExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitGivenStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitGivenCase(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTakeStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitReturnStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitLabel(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitGotoStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitBreakStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitContinueStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitThrowStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitTryStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitCatchClause(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitExpressionStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitNewExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitSelfExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitSuperExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitUnaryExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitParenExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitMemberExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitCallExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitBinaryExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitLogicalExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitAssignExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitUpdateExpression(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitDoStatement(node, ...) return self:visitStatement(node, ...) end
   function Visitor:visitArrayComprehension(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitComprehensionBlock(node, ...) return self:visitLoop(node, ...) end
   function Visitor:visitArrayLiteral(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitArrayPattern(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitPatternRule(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitPatternGrammar(node, ...) return self:visitDeclaration(node, ...) end
   function Visitor:visitPatternLiteral(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternAlternate(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternSequence(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternAny(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternAssert(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternAction(node, ...) return self:visitExpression(node, ...) end
   function Visitor:visitPatternRepeat(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternCaptSubst(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternCaptTable(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternCaptBasic(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternCaptConst(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternCaptGroup(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternCaptBack(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternCaptBackRef(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternReference(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternClass(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternRange(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternTerm(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternPredef(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitPatternArgument(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTypeName(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitFunctionType(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTypeCall(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTypeParams(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTypeVariance(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTypeList(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTypeGroup(node, ...) return self:visitNode(node, ...) end
   function Visitor:visitTypeUnion(node, ...) return self:visitNode(node, ...) end
end

local Dumper = { } do
   Dumper.__index = setmetatable(Dumper, Visitor)
   function Dumper.new()
      return setmetatable({
         buffer = { };
         level  = 0;
         line   = 0;
      }, Dumper)
   end
   function Dumper.dump(node)
      local dumper = Dumper.new()
      node:accept(dumper)
      return table.concat(dumper.buffer, "\n")
   end
   function Dumper:indent()
      self.level = self.level + 1
   end
   function Dumper:undent()
      self.level = self.level - 1
   end
   function Dumper:write(frag)
      self.buffer[#self.buffer + 1] = string.rep("  ", self.level)..tostring(frag)
   end
   function Dumper:visitNode(node)
      if node:get_line() then
         self.line = node:get_line()
      end
      self:write(node.tag..":"..self.line.."{")
      self:indent()
      node:visit_children(self)
      self:undent()
      self:write("},")
   end
   function Dumper:visitNodeList(list)
      self:write("[")
      self:indent()
      for i=1, #list do
         list[i]:accept(self)
      end
      self:undent()
      self:write("],")
   end
   function Dumper:visitIdentifier(node)
      self:write("Id("..node.name..")")
      if node.type then
         node.type:accept(self)
      end
   end
   function Dumper:visitLiteral(node)
      if type(node.value) == "string" then
         self:write(string.format("%q", node.value))
      else
         self:write(node.value)
      end
   end
end

local Node, NodeList

Node = { tag = "Node" } do
   Node.__index = Node
   Node.child_keys = { }

   function Node:children()
      local i = 0
      local function iter()
         i = i + 1
         if i <= #self.child_keys then
            if self[self.child_keys[i]] ~= nil then
               return self[self.child_keys[i]]
            else
               return iter()
            end
         end
      end
      return iter
   end

   function Node:visit_children(visitor, ...)
      for node in self:children() do
         if not node.accept then
            local util = require("blaze.lang.util")
            print(table.concat(visitor.buffer, "\n"))
         end
         node:accept(visitor, ...)
      end
   end

   function Node:set_line(line)
      self.line = line
   end
   function Node:get_line()
      return self.line
   end
   function Node:set_decorators(decos)
      self._decorators = NodeList.new(decos or { })
   end
   function Node:get_decorators()
      return self._decorators
   end
end

NodeList = { tag = "NodeList" } do
   NodeList.__index = setmetatable(NodeList, Node)
   function NodeList.new(nodes)
      return setmetatable(nodes, NodeList)
   end
   function NodeList:accept(visitor, ...)
      return visitor:visitNodeList(self, ...)
   end
   function NodeList:children()
      local i = 0
      return function()
         i = i + 1
         if i <= #self then
            return self[i]
         end
      end
   end
end

local Statement = { tag = "Statement" } do
   Statement.__index = setmetatable(Statement, Node)
end

local Expression = { tag = "Expression" } do
   Expression.__index = setmetatable(Expression, Node)
end

local Literal = { tag = "Literal" } do
   Literal.__index = setmetatable(Literal, Node)
   function Literal.new(value)
      return setmetatable({
         value = value
      }, Literal)
   end
   function Literal:accept(visitor, ...)
      return visitor:visitLiteral(self, ...)
   end
   function Literal:set_value(v)
      self.value = v
   end
   function Literal:get_value()
      return self.value
   end
   function Literal:to_string()
      return tostring(self.value)
   end
end

local Identifier = { tag = "Identifier" } do
   Identifier.__index = setmetatable(Identifier, Node)
   Identifier.child_keys = { 'type' }

   function Identifier.new(name, type)
      return setmetatable({
         name = name,
         type = type,
      }, Identifier)
   end

   function Identifier:accept(visitor, ...)
      return visitor:visitIdentifier(self, ...)
   end

   function Identifier:set_lookup(v)
      self._lookup = v
   end
   function Identifier:is_lookup()
      return self._lookup == true
   end
   function Identifier:get_symbol()
      return self.name
   end
end

local BlockNode = { tag = "BlockNode" } do
   BlockNode.__index = setmetatable(BlockNode, Node)
   BlockNode.child_keys = { 'body' }

   function BlockNode.new(body)
      return setmetatable({
         body = NodeList.new(body)
      }, BlockNode)
   end

   function BlockNode:accept(visitor, ...)
      return visitor:visitBlockNode(self, ...)
   end

end

local ChunkNode = { tag = "ChunkNode" } do
   ChunkNode.__index = setmetatable(ChunkNode, Node)
   ChunkNode.child_keys = { 'body' }

   function ChunkNode.new(body)
      return setmetatable({
         body = NodeList.new(body),
      }, ChunkNode)
   end

   function ChunkNode:accept(visitor, ...)
      return visitor:visitChunkNode(self, ...)
   end
end

local DecoratorNode = { tag = "DecoratorNode" } do
   DecoratorNode.__index = setmetatable(DecoratorNode, Expression)
   DecoratorNode.child_keys = { 'term' }

   function DecoratorNode.new(term)
      return setmetatable({ term = term }, DecoratorNode)
   end

   function DecoratorNode:accept(visitor, ...)
      return visitor:visitDecoratorNode(self, ...)
   end
end

local IncludeStatement = { tag = "IncludeStatement" } do
   IncludeStatement.__index = setmetatable(IncludeStatement, Statement)
   IncludeStatement.child_keys = { 'part' }

   function IncludeStatement.new(part)
      return setmetatable({
         part = part,
      }, IncludeStatement)
   end

   function IncludeStatement:accept(visitor, ...)
      return visitor:visitIncludeStatement(self, ...)
   end

end

local TraitNode = { tag = "TraitNode" } do
   TraitNode.__index = setmetatable(TraitNode, Node)
   TraitNode.child_keys = { 'name', 'base', 'body' }

   function TraitNode.new(name, base, body)
      return setmetatable({
         name = name,
         base = base,
         body = body,
      }, TraitNode)
   end

   function TraitNode:accept(visitor, ...)
      return visitor:visitTraitNode(self, ...)
   end

   function TraitNode:get_name()
      return self.name:get_symbol()
   end
end

local GrammarNode = { tag = "GrammarNode" } do
   GrammarNode.__index = setmetatable(GrammarNode, Node)
   GrammarNode.child_keys = { 'name', 'base', 'body' }

   function GrammarNode.new(name, base, body)
      return setmetatable({
         name = name,
         base = base,
         body = body,
      }, GrammarNode)
   end

   function GrammarNode:accept(visitor, ...)
      return visitor:visitGrammarNode(self, ...)
   end
end

local ClassNode = { tag = "ClassNode" } do
   ClassNode.__index = setmetatable(ClassNode, Node)
   ClassNode.child_keys = { 'name', 'args', 'base', 'body' }

   function ClassNode.new(name, args, base, body)
      return setmetatable({
         name = name,
         args = args,
         base = base,
         body = body,
      }, ClassNode)
   end
   function ClassNode:accept(visitor, ...)
      return visitor:visitClassNode(self, ...)
   end
   function ClassNode:get_name()
      return self.name:get_symbol()
   end
   function ClassNode:get_parameters()
      return self.args
   end
end

local PropertyNode = { tag = "PropertyNode" } do
   PropertyNode.__index = setmetatable(PropertyNode, Node)
   PropertyNode.child_keys = { 'name', 'type', 'init' }

   function PropertyNode.new(name, type, init)
      return setmetatable({
         name = name,
         type = type,
         init = init,
      }, PropertyNode)
   end
   function PropertyNode:accept(visitor, ...)
      return visitor:visitPropertyNode(self, ...)
   end
   function PropertyNode:get_type()
      return self.type
   end
   function PropertyNode:get_init()
      return self.init
   end
end

local MethodNode = { tag = "MethodNode" } do
   MethodNode.__index = setmetatable(MethodNode, Node)
   MethodNode.child_keys = { 'name', 'head', 'body' }

   function MethodNode.new(name, head, body)
      --head.params:ensure_self()
      return setmetatable({
         name = name,
         head = head,
         body = body,
      }, MethodNode)
   end
   function MethodNode:accept(visitor, ...)
      return visitor:visitMethodNode(self, ...)
   end
   function MethodNode:is_getter()
      return self._getter == true
   end
   function MethodNode:set_getter(v)
      self._getter = v
   end
   function MethodNode:is_setter()
      return self._setter == true
   end
   function MethodNode:set_setter(v)
      self._setter = v
   end
   function MethodNode:is_generator()
      return self._generator == true
   end
   function MethodNode:set_generator(v)
      self._generator = v
   end
   function MethodNode:get_name()
      return self.name:get_symbol()
   end
end

local ParameterNode = { tag = "ParameterNode" } do
   ParameterNode.__index = setmetatable(ParameterNode, Node)
   ParameterNode.child_keys = { 'name', 'type', 'init' }

   function ParameterNode.new(name, type, init, rest)
      return setmetatable({
         name = name,
         type = type,
         init = init,
         rest = rest,
      }, ParameterNode)
   end

   function ParameterNode:accept(visitor, ...)
      return visitor:visitParameterNode(self, ...)
   end

   function ParameterNode:get_name()
      return self.name:get_symbol()
   end

   function ParameterNode:get_type()
      return self.type
   end

   function ParameterNode:get_init()
      return self.init
   end

   function ParameterNode:is_rest()
      return self.rest == 'true'
   end
end

local ParameterList = { tag = "ParameterList" } do
   ParameterList.__index = setmetatable(ParameterList, NodeList)

   function ParameterList.new(params)
      return setmetatable(params, ParameterList)
   end

   function ParameterList:accept(visitor, ...)
      return visitor:visitParameterList(self, ...)
   end

   function ParameterList:has_rest()
      return #self > 0 and self[#self]:is_rest()
   end

   function ParameterList:has_self()
      return #self > 0 and self[1]:get_name() == "self"
   end

   function ParameterList:ensure_self()
      if not self:has_self() then
         local ident = Identifier.new("self")
         table.insert(self, 1, ParameterNode.new(ident))
      end
   end
end

local SignatureNode = { tag = "SignatureNode" } do
   SignatureNode.__index = setmetatable(SignatureNode, Node)
   SignatureNode.child_keys = { 'params', 'returns' }

   function SignatureNode.new(params, returns)
      return setmetatable({
         params = params,
         returns = returns,
      }, SignatureNode)
   end

   function SignatureNode:accept(visitor, ...)
      return visitor:visitSignatureNode(self, ...)
   end
end

local FunctionNode = { tag = "FunctionNode" } do
   FunctionNode.__index = setmetatable(FunctionNode, Node)
   FunctionNode.child_keys = { 'name', 'head', 'body' }

   function FunctionNode.new(name, head, body)
      return setmetatable({
         name = name,
         head = head,
         body = body,
      }, FunctionNode)
   end
   function FunctionNode:accept(visitor, ...)
      return visitor:visitFunctionNode(self, ...)
   end
   function FunctionNode:set_local(v)
      self._local = v
   end
   function FunctionNode:is_local()
      return self._local == true
   end
   function FunctionNode:set_generator(v)
      self._generator = v
   end
   function FunctionNode:is_generator()
      return self._generator == true
   end
   function FunctionNode:set_expression(v)
      self._expression = v
   end
   function FunctionNode:is_expression()
      return self._expression == true
   end
   function FunctionNode:get_name()
      return self.name:get_symbol()
   end
end

local MacroNode = { tag = "MacroNode" } do
   MacroNode.__index = setmetatable(MacroNode, Node)
   MacroNode.child_keys = { 'name', 'head', 'body' }

   function MacroNode.new(name, head, body)
      return setmetatable({
         name = name,
         head = head,
         body = body,
      }, MacroNode)
   end
   function MacroNode:accept(visitor, ...)
      return visitor:visitMacroNode(self, ...)
   end
end

local ModuleStatement = { tag = "ModuleStatement" } do
   ModuleStatement.__index = setmetatable(ModuleStatement, Statement)
   ModuleStatement.child_keys = { 'path' }

   function ModuleStatement.new(path)
      return setmetatable({
         path = NodeList.new(path)
      }, ModuleStatement)
   end

   function ModuleStatement:accept(visitor, ...)
      return visitor:visitModuleStatement(self, ...)
   end

   function ModuleStatement:get_name()
      local buf = { }
      for i, n in self.path do
         buf[#buf + 1] = n:get_symbol()
      end
      return table.concat(buf, ".")
   end
end

local ImportStatement = { tag = "ImportStatement" } do
   ImportStatement.__index = setmetatable(ImportStatement, Statement)
   ImportStatement.child_keys = { 'terms', 'from' }

   function ImportStatement.new(terms, from)
      return setmetatable({
         terms = NodeList.new(terms),
         from  = from,
      }, ImportStatement)
   end

   function ImportStatement:accept(visitor, ...)
      return visitor:visitImportStatement(self, ...)
   end

   function ImportStatement:get_path()
      return self.from:to_string()
   end
end

local ImportTerm = { tag = "ImportTerm" } do
   ImportTerm.__index = setmetatable(ImportTerm, Expression)
   ImportTerm.child_keys = { 'name', 'alias' }

   function ImportTerm.new(name, alias)
      return setmetatable({
         name  = name,
         alias = alias,
      }, ImportTerm)
   end

   function ImportTerm:accept(visitor, ...)
      return visitor:visitImportTerm(self, ...)
   end
end

local ExportStatement = { tag = "ExportStatement" } do
   ExportStatement.__index = setmetatable(ExportStatement, Statement)
   ExportStatement.child_keys = { 'names' }

   function ExportStatement.new(names)
      return setmetatable({
         names = NodeList.new(names)
      }, ExportStatement)
   end

   function ExportStatement:accept(visitor, ...)
      return visitor:visitExportStatement(self, ...)
   end
end

local LocalDeclaration = { tag = "LocalDeclaration" } do
   LocalDeclaration.__index = setmetatable(LocalDeclaration, Statement)
   LocalDeclaration.child_keys = { 'names', 'inits' }

   function LocalDeclaration.new(names, inits)
      return setmetatable({
         names = NodeList.new(names),
         inits = NodeList.new(inits),
      }, LocalDeclaration)
   end
   function LocalDeclaration:accept(visitor, ...)
      return visitor:visitLocalDeclaration(self, ...)
   end
end

local InExpression = { tag = "InExpression" } do
   InExpression.__index = setmetatable(InExpression, Expression)
   InExpression.child_keys = { 'names', 'expression' }

   function InExpression.new(names, expression)
      return setmetatable({
         names = NodeList.new(names),
         expression = expression,
      }, InExpression)
   end

   function InExpression:accept(visitor, ...)
      return visitor:visitInExpression(self, ...)
   end
end

local RichString = { tag = "RichString" } do
   RichString.__index = setmetatable(RichString, Node)
   RichString.child_keys = { 'terms' }

   function RichString.new(exprs)
      return setmetatable({
         terms = NodeList.new(exprs)
      }, RichString)
   end

   function RichString:accept(visitor, ...)
      return visitor:visitRichString(self, ...)
   end

   function RichString:to_string()
      local buf = { }
      for i=1, #self.terms do
         -- FIXME: this assumes a list of literals only
         buf[#buf + 1] = self.terms[i]:to_string()
      end
      return table.concat(buf)
   end
end

local TablePattern = { tag = "TablePattern" } do
   TablePattern.__index = setmetatable(TablePattern, Node)
   TablePattern.child_keys = { 'entries', 'coerce' }

   function TablePattern.new(entries, coerce)
      return setmetatable({
         entries = NodeList.new(entries),
         coerce  = coerce,
      }, TablePattern)
   end

   function TablePattern:accept(visitor, ...)
      return visitor:visitTablePattern(self, ...)
   end
end

local TableItem = { tag = "TableItem" } do
   TableItem.__index = setmetatable(TableItem, Node)
   TableItem.child_keys = { 'value' }

   function TableItem.new(item)
      return setmetatable(item, TableItem)
   end
   function TableItem:accept(visitor, ...)
      return visitor:visitTableItem(self, ...)
   end
end

local TablePair = { tag = "TablePair" } do
   TablePair.__index = setmetatable(TablePair, Node)
   TablePair.child_keys = { 'name', 'expr', 'value' }

   function TablePair.new(pair)
      return setmetatable(pair, TablePair)
   end
   function TablePair:accept(visitor, ...)
      return visitor:visitTablePair(self, ...)
   end
end

local TableLiteral = { tag = "TableLiteral" } do
   TableLiteral.__index = setmetatable(TableLiteral, Node)
   TableLiteral.child_keys = { 'entries' }

   function TableLiteral.new(entries)
      return setmetatable({
         entries = NodeList.new(entries),
      }, TableLiteral)
   end

   function TableLiteral:accept(visitor, ...)
      return visitor:visitTableLiteral(self, ...)
   end
end

local IfStatement = { tag = "IfStatement" } do
   IfStatement.__index = setmetatable(IfStatement, Statement)
   IfStatement.child_keys = { 'test', 'consequent', 'alternate' }

   function IfStatement.new(test, cons, altn)
      return setmetatable({
         test       = test,
         consequent = cons,
         alternate  = altn,
      }, IfStatement)
   end
   function IfStatement:accept(visitor, ...)
      return visitor:visitIfStatement(self, ...)
   end
end

local WhileStatement = { tag = "WhileStatement" } do
   WhileStatement.__index = setmetatable(WhileStatement, Statement)
   WhileStatement.child_keys = { 'test', 'body' }

   function WhileStatement.new(test, body)
      return setmetatable({
         test = test,
         body = body,
      }, WhileStatement)
   end

   function WhileStatement:accept(visitor, ...)
      return visitor:visitWhileStatement(self, ...)
   end
end

local RepeatStatement = { tag = "RepeatStatement" } do
   RepeatStatement.__index = setmetatable(RepeatStatement, Statement)
   RepeatStatement.child_keys = { 'test', 'body' }

   function RepeatStatement.new(test, body)
      return setmetatable({
         test = test,
         body = body,
      }, RepeatStatement)
   end

   function RepeatStatement:accept(visitor, ...)
      return visitor:visitRepeatStatement(self, ...)
   end
end

local ForStatement = { tag = "ForStatement" } do
   ForStatement.__index = setmetatable(ForStatement, Statement)
   ForStatement.child_keys = { 'name', 'init', 'last', 'step', 'body' }

   function ForStatement.new(name, init, last, step, body)
      return setmetatable({
         name = name,
         init = init,
         last = last,
         step = step,
         body = body,
      }, ForStatement)
   end

   function ForStatement:accept(visitor, ...)
      return visitor:visitForStatement(self, ...)
   end
end

local ForInStatement = { tag = "ForInStatement" } do
   ForInStatement.__index = setmetatable(ForInStatement, Statement)
   ForInStatement.child_keys = { 'left', 'right', 'body' }

   function ForInStatement.new(left, right, body)
      return setmetatable({
         left  = NodeList.new(left),
         right = right,
         body  = body,
      }, ForInStatement)
   end
   function ForInStatement:accept(visitor, ...)
      return visitor:visitForInStatement(self, ...)
   end
end

local SpreadExpression = { tag = "SpreadExpression" } do
   SpreadExpression.__index = setmetatable(SpreadExpression, Expression)
   SpreadExpression.child_keys = { 'argument' }

   function SpreadExpression.new(argument)
      return setmetatable({
         argument = argument
      }, SpreadExpression)
   end
   function SpreadExpression:accept(visitor, ...)
      return visitor:visitSpreadExpression(self, ...)
   end
end

local GivenStatement = { tag = "GivenStatement" } do
   GivenStatement.__index = setmetatable(GivenStatement, Statement)
   GivenStatement.child_keys = { 'discriminant', 'cases' }

   function GivenStatement.new(discriminant, cases)
      return setmetatable({
         discriminant = discriminant,
         cases = NodeList.new(cases),
      }, GivenStatement)
   end
   function GivenStatement:accept(visitor, ...)
      return visitor:visitGivenStatement(self, ...)
   end
end

local GivenCase = { tag = "GivenCase" } do
   GivenCase.__index = setmetatable(GivenCase, Statement)
   GivenCase.child_keys = { 'test', 'guard', 'consequent' }

   function GivenCase.new(test, guard, cons)
      return setmetatable({
         test = test,
         guard = guard,
         consequent = cons,
      }, GivenCase)
   end
   function GivenCase:accept(visitor, ...)
      return visitor:visitGivenCase(self, ...)
   end
end

local TakeStatement = { tag = "TakeStatement" } do
   TakeStatement.__index = setmetatable(TakeStatement, Statement)
   TakeStatement.child_keys = { 'arguments' }

   function TakeStatement.new(arguments)
      return setmetatable({
         arguments = NodeList.new(arguments),
      }, TakeStatement)
   end
   function TakeStatement:accept(visitor, ...)
      return visitor:visitTakeStatement(self, ...)
   end
end

local ReturnStatement = { tag = "ReturnStatement" } do
   ReturnStatement.__index = setmetatable(ReturnStatement, Statement)
   ReturnStatement.child_keys = { 'arguments' }
   function ReturnStatement.new(arguments)
      return setmetatable({
         arguments = NodeList.new(arguments),
      }, ReturnStatement)
   end
   function ReturnStatement:accept(visitor, ...)
      return visitor:visitReturnStatement(self, ...)
   end
end

local Label = { tag = "Label" } do
   Label.__index = setmetatable(Label, Node)
   function Label.new(label)
      return setmetatable({
         label = label
      }, Label)
   end
   function Label:accept(visitor, ...)
      return visitor:visitLabel(self, ...)
   end
end

local GotoStatement = { tag = "GotoStatement" } do
   GotoStatement.__index = setmetatable(GotoStatement, Statement)
   function GotoStatement.new(label)
      return setmetatable({
         label = label
      }, GotoStatement)
   end
   function GotoStatement:accept(visitor, ...)
      return visitor:visitGotoStatement(self, ...)
   end
end

local BreakStatement = { tag = "BreakStatement" } do
   BreakStatement.__index = setmetatable(BreakStatement, Statement)
   function BreakStatement.new()
      return setmetatable({ }, BreakStatement)
   end
   function BreakStatement:accept(visitor, ...)
      return visitor:visitBreakStatement(self, ...)
   end
end

local ContinueStatement = { tag = "ContinueStatement" } do
   ContinueStatement.__index = setmetatable(ContinueStatement, Statement)
   function ContinueStatement.new()
      return setmetatable({ }, ContinueStatement)
   end
   function ContinueStatement:accept(visitor, ...)
      return visitor:visitContinueStatement(self, ...)
   end
end

local ThrowStatement = { tag = "ThrowStatement" } do
   ThrowStatement.__index = setmetatable(ThrowStatement, Statement)
   ThrowStatement.child_keys = { 'argument' }
   function ThrowStatement.new(argument)
      return setmetatable({
         argument = argument,
      }, ThrowStatement)
   end
   function ThrowStatement:accept(visitor, ...)
      return visitor:visitThrowStatement(self, ...)
   end
end

local TryStatement = { tag = "TryStatement" } do
   TryStatement.__index = setmetatable(TryStatement, Statement)
   TryStatement.child_keys = { 'body', 'handlers', 'finalizer' }

   function TryStatement.new(body, handlers, finalizer)
      return setmetatable({
         body = body,
         handlers = NodeList.new(handlers),
         finalizer = finalizer,
      }, TryStatement)
   end
   function TryStatement:accept(visitor, ...)
      return visitor:visitTryStatement(self, ...)
   end
end

local CatchClause = { tag = "CatchClause" } do
   CatchClause.__index = setmetatable(CatchClause, Node)
   CatchClause.child_keys = { 'param', 'guard', 'body' }

   function CatchClause.new(param, guard, body)
      return setmetatable({
         param = param,
         guard = guard,
         body  = body,
      }, CatchClause)
   end
   function CatchClause:accept(visitor, ...)
      return visitor:visitCatchClause(self, ...)
   end
end

local ExpressionStatement = { tag = "ExpressionStatement" } do
   ExpressionStatement.__index = ExpressionStatement
   ExpressionStatement.child_keys = { 'expression' }

   setmetatable(ExpressionStatement, Statement)

   function ExpressionStatement.new(expr)
      return setmetatable({
         expression = expr,
      }, ExpressionStatement)
   end

   function ExpressionStatement:accept(visitor, ...)
      return visitor:visitExpressionStatement(self, ...)
   end
end

local NewExpression = { tag = "NewExpression" } do
   NewExpression.__index = setmetatable(NewExpression, Expression)
   NewExpression.child_keys = { 'base', 'types', 'arguments' }

   function NewExpression.new(base, types, args)
      return setmetatable({
         base = base,
         types = types,
         arguments = NodeList.new(args),
      }, NewExpression)
   end
   function NewExpression:accept(visitor, ...)
      return visitor:visitNewExpression(self, ...)
   end
end

local SelfExpression = { tag = "SelfExpression" } do
   SelfExpression.__index = setmetatable(SelfExpression, Expression)
   function SelfExpression.new()
      return setmetatable({ }, SelfExpression)
   end
   function SelfExpression:accept(visitor, ...)
      return visitor:visitSelfExpression(self, ...)
   end
end

local SuperExpression = { tag = "SuperExpression" } do
   SuperExpression.__index = setmetatable(SuperExpression, Expression)
   function SuperExpression.new()
      return setmetatable({ }, SuperExpression)
   end
   function SuperExpression:accept(visitor, ...)
      return visitor:visitSuperExpression(self, ...)
   end
end

local UnaryExpression = { tag = "UnaryExpression" } do
   UnaryExpression.__index = setmetatable(UnaryExpression, Expression)
   UnaryExpression.child_keys = { 'argument' }
   function UnaryExpression.new(oper, term)
      return setmetatable({
         operator = oper,
         argument = term,
      }, UnaryExpression)
   end
   function UnaryExpression:accept(visitor, ...)
      return visitor:visitUnaryExpression(self, ...)
   end
end

local ParenExpression = { tag = "ParenExpression" } do
   ParenExpression.__index = setmetatable(ParenExpression, Expression)
   ParenExpression.child_keys = { 'expression' }

   function ParenExpression.new(expr)
      return setmetatable({
         expression = expr,
      }, ParenExpression)
   end
   function ParenExpression:accept(visitor, ...)
      return visitor:visitParenExpression(self, ...)
   end
end

local MemberExpression = { tag = "MemberExpression" } do
   MemberExpression.__index = setmetatable(MemberExpression, Expression)
   MemberExpression.child_keys = { 'object', 'property' }

   function MemberExpression.new(object, property, computed)
      return setmetatable({
         object   = object,
         property = property,
         computed = computed
      }, MemberExpression)
   end
   function MemberExpression:accept(visitor, ...)
      return visitor:visitMemberExpression(self, ...)
   end

   function MemberExpression:is_computed()
      return self.computed == true
   end
end

local CallExpression = { tag = "CallExpression" } do
   CallExpression.__index = setmetatable(CallExpression, Expression)
   CallExpression.child_keys = { 'callee', 'arguments' }

   function CallExpression.new(callee, arguments)
      return setmetatable({
         callee    = callee,
         arguments = NodeList.new(arguments),
      }, CallExpression)
   end
   function CallExpression:accept(visitor, ...)
      return visitor:visitCallExpression(self, ...)
   end
end

local BinaryExpression = { tag = "BinaryExpression" } do
   BinaryExpression.__index = setmetatable(BinaryExpression, Expression)
   BinaryExpression.child_keys = { 'left', 'right' }

   function BinaryExpression.new(oper, left, right)
      return setmetatable({
         operator = oper,
         left     = left,
         right    = right,
      }, BinaryExpression)
   end
   function BinaryExpression:accept(visitor, ...)
      return visitor:visitBinaryExpression(self, ...)
   end
end

local LogicalExpression = { tag = "LogicalExpression" } do
   LogicalExpression.__index = setmetatable(LogicalExpression, Expression)
   LogicalExpression.child_keys = { 'left', 'right' }

   function LogicalExpression.new(oper, left, right)
      return setmetatable({
         operator = oper,
         left     = left,
         right    = right,
      }, LogicalExpression)
   end
   function LogicalExpression:accept(visitor, ...)
      return visitor:visitLogicalExpression(self, ...)
   end
end

local AssignExpression = { tag = "AssignExpression" } do
   AssignExpression.__index = setmetatable(AssignExpression, Expression)
   AssignExpression.child_keys = { 'left', 'right' }

   function AssignExpression.new(oper, left, right)
      return setmetatable({
         operator = oper,
         left     = NodeList.new(left),
         right    = NodeList.new(right),
      }, AssignExpression)
   end
   function AssignExpression:accept(visitor, ...)
      return visitor:visitAssignExpression(self, ...)
   end
end

local UpdateExpression = { tag = "UpdateExpression" } do
   UpdateExpression.__index = setmetatable(UpdateExpression, Expression)
   UpdateExpression.child_keys = { 'left', 'right' }

   function UpdateExpression.new(oper, left, right)
      return setmetatable({
         operator = oper,
         left     = left,
         right    = right,
      }, UpdateExpression)
   end
   function UpdateExpression:accept(visitor, ...)
      return visitor:visitUpdateExpression(self, ...)
   end
end

local DoStatement = { tag = "DoStatement" } do
   DoStatement.__index = setmetatable(DoStatement, Statement)
   DoStatement.child_keys = { 'body' }

   function DoStatement.new(body)
      return setmetatable({
         body = body,
      }, DoStatement)
   end
   function DoStatement:accept(visitor, ...)
      return visitor:visitDoStatement(self, ...)
   end
end

local ArrayComprehension = { tag = "ArrayComprehension" } do
   ArrayComprehension.__index = setmetatable(ArrayComprehension, Node)
   ArrayComprehension.child_keys = { 'body', 'blocks' }

   function ArrayComprehension.new(body, blocks)
      return setmetatable({
         body   = NodeList.new(body),
         blocks = NodeList.new(blocks),
      }, ArrayComprehension)
   end
   function ArrayComprehension:accept(visitor, ...)
      visitor:acceptArrayComprehension(self)
   end
end

local ComprehensionBlock = { tag = "ComprehensionBlock" } do
   ComprehensionBlock.__index = setmetatable(ComprehensionBlock, Node)
   ComprehensionBlock.child_keys = { 'left', 'right', 'filter' }

   function ComprehensionBlock.new(left, right, filter)
      return setmetatable({
         left = left,
         right = right,
         filter = filter,
      }, ComprehensionBlock)
   end
   function ComprehensionBlock:accept(visitor, ...)
      return visitor:visitComprehensionBlock(self, ...)
   end
end

local ArrayLiteral = { tag = "ArrayLiteral" } do
   ArrayLiteral.__index = setmetatable(ArrayLiteral, Literal)
   ArrayLiteral.child_keys = { 'elements' }

   function ArrayLiteral.new(elements)
      return setmetatable({
         elements = NodeList.new(elements),
      }, ArrayLiteral)
   end
   function ArrayLiteral:accept(visitor, ...)
      return visitor:visitArrayLiteral(self, ...)
   end
end

local ArrayPattern = { tag = "ArrayPattern" } do
   ArrayPattern.__index = setmetatable(ArrayPattern, Node)
   ArrayPattern.child_keys = { 'elements' }

   function ArrayPattern.new(elements)
      return setmetatable({
         elements = NodeList.new(elements),
      }, ArrayPattern)
   end
   function ArrayPattern:accept(visitor, ...)
      return visitor:visitArrayPattern(self, ...)
   end
end

local PatternRule = { tag = "PatternRule" } do
   PatternRule.__index = setmetatable(PatternRule, Node)
   PatternRule.child_keys = { 'name', 'body' }

   function PatternRule.new(name, body)
      return setmetatable({
         name = name,
         body = body,
      }, PatternRule)
   end
   function PatternRule:accept(visitor, ...)
      visitor:acceptPatternRule(self)
   end
end

local PatternGrammar = { tag = "PatternGrammar" } do
   PatternGrammar.__index = setmetatable(PatternGrammar, Node)
   PatternGrammar.child_keys = { 'rules' }

   function PatternGrammar.new(rules)
      return setmetatable({
         rules = NodeList.new(rules)
      }, PatternGrammar)
   end
   function PatternGrammar:accept(visitor, ...)
      return visitor:visitPatternGrammar(self, ...)
   end
end

local PatternLiteral = { tag = "PatternLiteral" } do
   PatternLiteral.__index = setmetatable(PatternLiteral, Literal)
   PatternLiteral.child_keys = { 'pattern' }

   function PatternLiteral.new(pattern)
      return setmetatable({
         pattern = pattern,
      }, PatternLiteral)
   end
   function PatternLiteral:accept(visitor, ...)
      return visitor:visitPatternLiteral(self, ...)
   end
end

local PatternAlternate = { tag = "PatternAlternate" } do
   PatternAlternate.__index = setmetatable(PatternAlternate, Node)
   PatternAlternate.child_keys = { 'left', 'right' }

   function PatternAlternate.new(left, right)
      return setmetatable({
         left  = left,
         right = right,
      }, PatternAlternate)
   end
   function PatternAlternate:accept(visitor, ...)
      return visitor:visitPatternAlternate(self, ...)
   end
end

local PatternSequence = { tag = "PatternSequence" } do
   PatternSequence.__index = setmetatable(PatternSequence, Node)
   PatternSequence.child_keys = { 'left', 'right' }

   function PatternSequence.new(left, right)
      return setmetatable({
         left  = left,
         right = right,
      }, PatternSequence)
   end
   function PatternSequence:accept(visitor, ...)
      return visitor:visitPatternSequence(self, ...)
   end
end

local PatternAny = { tag = "PatternAny" } do
   PatternAny.__index = setmetatable(PatternAny, Node)
   function PatternAny.new()
      return setmetatable({ }, PatternAny)
   end
   function PatternAny:accept(visitor, ...)
      return visitor:visitPatternAny(self, ...)
   end
end

local PatternAssert = { tag = "PatternAssert" } do
   PatternAssert.__index = setmetatable(PatternAssert, Node)
   PatternAssert.child_keys = { 'argument' }

   function PatternAssert.new(operator, argument)
      return setmetatable({
         operator = operator,
         argument = argument,
      }, PatternAssert)
   end
   function PatternAssert:accept(visitor, ...)
      return visitor:visitPatternAssert(self, ...)
   end
end

local PatternAction = { tag = "PatternAction" } do
   PatternAction.__index = setmetatable(PatternAction, Node)
   PatternAction.child_keys = { 'argument' }

   function PatternAction.new(operator, argument)
      return setmetatable({
         operator = operator,
         argument = argument,
      }, PatternAction)
   end
   function PatternAction:accept(visitor, ...)
      return visitor:visitPatternAction(self, ...)
   end
end

local PatternRepeat = { tag = "PatternRepeat" } do
   PatternRepeat.__index = setmetatable(PatternRepeat, Node)
   function PatternRepeat.new(count)
      return setmetatable({
         count = count
      }, PatternRepeat)
   end
   function PatternRepeat:accept(visitor, ...)
      return visitor:visitPatternRepeat(self, ...)
   end
end

local PatternCaptSubst = { tag = "PatternCaptSubst" } do
   PatternCaptSubst.__index = setmetatable(PatternCaptSubst, Node)
   function PatternCaptSubst.new(count)
      return setmetatable({
         count = count
      }, PatternCaptSubst)
   end
   function PatternCaptSubst:accept(visitor, ...)
      return visitor:visitPatternCaptSubst(self, ...)
   end
end

local PatternCaptTable = { tag = "PatternCaptTable" } do
   PatternCaptTable.__index = setmetatable(PatternCaptTable, Node)
   PatternCaptTable.child_keys = { 'pattern' }

   function PatternCaptTable.new(pattern)
      return setmetatable({
         pattern = pattern
      }, PatternCaptTable)
   end
   function PatternCaptTable:accept(visitor, ...)
      return visitor:visitPatternCaptTable(self, ...)
   end
end

local PatternCaptBasic = { tag = "PatternCaptBasic" } do
   PatternCaptBasic.__index = setmetatable(PatternCaptBasic, Node)
   PatternCaptBasic.child_keys = { 'pattern' }

   function PatternCaptBasic.new(pattern)
      return setmetatable({
         pattern = pattern
      }, PatternCaptBasic)
   end
   function PatternCaptBasic:accept(visitor, ...)
      return visitor:visitPatternCaptBasic(self, ...)
   end
end

local PatternCaptConst = { tag = "PatternCaptConst" } do
   PatternCaptConst.__index = setmetatable(PatternCaptConst, Node)
   PatternCaptConst.child_keys = { 'argument' }

   function PatternCaptConst.new(argument)
      return setmetatable({
         argument = argument
      }, PatternCaptConst)
   end
   function PatternCaptConst:accept(visitor, ...)
      return visitor:visitPatternCaptConst(self, ...)
   end
end

local PatternCaptGroup = { tag = "PatternCaptGroup" } do
   PatternCaptGroup.__index = setmetatable(PatternCaptGroup, Node)
   PatternCaptGroup.child_keys = { 'pattern' }

   function PatternCaptGroup.new(name, pattern)
      return setmetatable({
         name    = name,
         pattern = pattern,
      }, PatternCaptGroup)
   end
   function PatternCaptGroup:accept(visitor, ...)
      return visitor:visitPatternCaptGroup(self, ...)
   end
end

local PatternCaptBack = { tag = "PatternCaptBack" } do
   PatternCaptBack.__index = setmetatable(PatternCaptBack, Node)

   function PatternCaptBack.new(name)
      return setmetatable({
         name = name,
      }, PatternCaptBack)
   end
   function PatternCaptBack:accept(visitor, ...)
      return visitor:visitPatternCaptBack(self, ...)
   end
end

local PatternCaptBackRef = { tag = "PatternCaptBackRef" } do
   PatternCaptBackRef.__index = setmetatable(PatternCaptBackRef, Node)
   function PatternCaptBackRef.new(name)
      return setmetatable({
         name = name,
      }, PatternCaptBackRef)
   end
   function PatternCaptBackRef:accept(visitor, ...)
      return visitor:visitPatternCaptBackRef(self, ...)
   end
end

local PatternReference = { tag = "PatternReference" } do
   PatternReference.__index = setmetatable(PatternReference, Node)
   function PatternReference.new(name)
      return setmetatable({
         name = name,
      }, PatternReference)
   end
   function PatternReference:accept(visitor, ...)
      return visitor:visitPatternReference(self, ...)
   end
end

local PatternClass = { tag = "PatternClass" } do
   PatternClass.__index = setmetatable(PatternClass, Node)
   PatternClass.child_keys = { 'alternates' }

   function PatternClass.new(alternates, negated)
      return setmetatable({
         alternates = alternates,
         negated    = negated,
      }, PatternClass)
   end
   function PatternClass:accept(visitor, ...)
      return visitor:visitPatternClass(self, ...)
   end
   function PatternClass:is_negated()
      return self.negated == true
   end
end

local PatternRange = { tag = "PatternRange" } do
   PatternRange.__index = setmetatable(PatternRange, Node)
   PatternRange.child_keys = { 'left', 'right' }

   function PatternRange.new(left, right)
      return setmetatable({
         left  = left,
         right = right,
      }, PatternRange)
   end
   function PatternRange:accept(visitor, ...)
      return visitor:visitPatternRange(self, ...)
   end
end

local PatternTerm = { tag = "PatternTerm" } do
   PatternTerm.__index = setmetatable(PatternTerm, Node)
   function PatternTerm.new(literal)
      return setmetatable({
         literal = literal,
      }, PatternTerm)
   end
   function PatternTerm:accept(visitor, ...)
      return visitor:visitPatternTerm(self, ...)
   end
end

local PatternPredef = { tag = "PatternPredef" } do
   PatternPredef.__index = setmetatable(PatternPredef, Node)
   function PatternPredef.new(name)
      return setmetatable({
         name = name,
      }, PatternPredef)
   end
   function PatternPredef:accept(visitor, ...)
      return visitor:visitPatternPredef(self, ...)
   end
end

local PatternArgument = { tag = "PatternArgument" } do
   PatternArgument.__index = setmetatable(PatternArgument, Node)
   function PatternArgument.new(index)
      return setmetatable({
         index = index,
      }, PatternArgument)
   end
   function PatternArgument:accept(visitor, ...)
      return visitor:visitPatternArgument(self, ...)
   end
end

local FunctionType = { tag = "FunctionType" } do
   FunctionType.__index = setmetatable(FunctionType, Node)
   FunctionType.child_keys = { 'params', 'returns' }

   function FunctionType.new(params, returns)
      return setmetatable({
         params = params,
         returns = returns,
      }, FunctionType)
   end

   function FunctionType:accept(visitor, ...)
      return visitor:visitFunctionType(self, ...)
   end
end

local TypeVariance = { tag = "TypeVariance" } do
   TypeVariance.__index = setmetatable(TypeVariance, Node)
   TypeVariance.child_keys = { 'name', 'base' }

   function TypeVariance.new(name, base)
      return setmetatable({
         name = name,
         base = base,
      }, TypeVariance)
   end
   function TypeVariance:accept(visitor, ...)
      return visitor:visitTypeVariance(self, ...)
   end
end

local TypeName = { tag = "TypeName" } do
   TypeName.__index = setmetatable(TypeName, Node)
   TypeName.child_keys = { 'base', 'arguments' }

   function TypeName.new(base, args)
      return setmetatable({
         base = base,
         arguments = args,
      }, TypeName)
   end
   function TypeName:accept(visitor, ...)
      return visitor:visitTypeName(self, ...)
   end
end

local TypeParams = { tag = "TypeParams" } do
   TypeParams.__index = setmetatable(TypeParams, NodeList)
   function TypeParams.new(list)
      return setmetatable(list, TypeParams)
   end
   function TypeParams:accept(visitor, ...)
      return visitor:visitTypeParams(self, ...)
   end
end

local TypeList = { tag = "TypeList" } do
   TypeList.__index = setmetatable(TypeList, Node)
   TypeList.child_keys = { 'elements' }

   function TypeList.new(elements)
      return setmetatable({
         elements = NodeList.new(elements),
      }, TypeList)
   end
   function TypeList:accept(visitor, ...)
      return visitor:visitTypeList(self, ...)
   end
end

local TypeGroup = { tag = "TypeGroup" } do
   TypeGroup.__index = setmetatable(TypeGroup, Node)
   TypeGroup.child_keys = { 'expression' }

   function TypeGroup.new(expression)
      return setmetatable({
         expression = expression
      }, TypeGroup)
   end
   function TypeGroup:accept(visitor, ...)
      return visitor:visitTypeGroup(self, ...)
   end
end

local TypeUnion = { tag = "TypeUnion" } do
   TypeUnion.__index = setmetatable(TypeUnion, Node)
   TypeUnion.child_keys = { 'left', 'right' }

   function TypeUnion.new(left, right)
      return setmetatable({
         left = left,
         right = right,
      }, TypeUnion)
   end
   function TypeUnion:accept(visitor, ...)
      return visitor:visitTypeUnion(self, ...)
   end
end


return {
   Visitor = Visitor,
   Dumper = Dumper,
   Node = Node,
   Statement = Statement,
   Expression = Expression,
   Literal = Literal,
   NodeList = NodeList,
   BlockNode = BlockNode,
   ChunkNode = ChunkNode,
   DecoratorNode = DecoratorNode,
   IncludeStatement = IncludeStatement,
   TraitNode = TraitNode,
   GrammarNode = GrammarNode,
   ClassNode = ClassNode,
   PropertyNode = PropertyNode,
   MethodNode = MethodNode,
   ParameterNode = ParameterNode,
   ParameterList = ParameterList,
   SignatureNode = SignatureNode,
   FunctionNode = FunctionNode,
   MacroNode = MacroNode,
   ModuleStatement = ModuleStatement,
   ImportStatement = ImportStatement,
   ImportTerm = ImportTerm,
   ExportStatement = ExportStatement,
   Identifier = Identifier,
   LocalDeclaration = LocalDeclaration,
   InExpression = InExpression,
   RichString = RichString,
   TablePattern = TablePattern,
   TableLiteral = TableLiteral,
   TableItem = TableItem,
   TablePair = TablePair,
   IfStatement = IfStatement,
   WhileStatement = WhileStatement,
   RepeatStatement = RepeatStatement,
   ForStatement = ForStatement,
   ForInStatement = ForInStatement,
   SpreadExpression = SpreadExpression,
   GivenStatement = GivenStatement,
   GivenCase = GivenCase,
   TakeStatement = TakeStatement,
   ReturnStatement = ReturnStatement,
   Label = Label,
   GotoStatement = GotoStatement,
   BreakStatement = BreakStatement,
   ContinueStatement = ContinueStatement,
   ThrowStatement = ThrowStatement,
   TryStatement = TryStatement,
   CatchClause = CatchClause,
   ExpressionStatement = ExpressionStatement,
   NewExpression = NewExpression,
   SelfExpression = SelfExpression,
   SuperExpression = SuperExpression,
   UnaryExpression = UnaryExpression,
   ParenExpression = ParenExpression,
   MemberExpression = MemberExpression,
   CallExpression = CallExpression,
   BinaryExpression = BinaryExpression,
   LogicalExpression = LogicalExpression,
   AssignExpression = AssignExpression,
   UpdateExpression = UpdateExpression,
   DoStatement = DoStatement,
   ArrayComprehension = ArrayComprehension,
   ComprehensionBlock = ComprehensionBlock,
   ArrayLiteral = ArrayLiteral,
   ArrayPattern = ArrayPattern,
   PatternRule = PatternRule,
   PatternGrammar = PatternGrammar,
   PatternLiteral = PatternLiteral,
   PatternAlternate = PatternAlternate,
   PatternSequence = PatternSequence,
   PatternAny = PatternAny,
   PatternAssert = PatternAssert,
   PatternAction = PatternAction,
   PatternRepeat = PatternRepeat,
   PatternCaptSubst = PatternCaptSubst,
   PatternCaptTable = PatternCaptTable,
   PatternCaptBasic = PatternCaptBasic,
   PatternCaptConst = PatternCaptConst,
   PatternCaptGroup = PatternCaptGroup,
   PatternCaptBack = PatternCaptBack,
   PatternCaptBackRef = PatternCaptBackRef,
   PatternReference = PatternReference,
   PatternClass = PatternClass,
   PatternRange = PatternRange,
   PatternTerm = PatternTerm,
   PatternPredef = PatternPredef,
   PatternArgument = PatternArgument,
   TypeName = TypeName,
   TypeCall = TypeCall,
   TypeParams = TypeParams,
   TypeVariance = TypeVariance,
   FunctionType = FunctionType,
   TypeList = TypeList,
   TypeGroup = TypeGroup,
   TypeUnion = TypeUnion,
}
