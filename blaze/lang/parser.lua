--[=[
Copyright (C) 2013-2014 Richard Hundt and contributors.
See Copyright Notice in blaze
]=]

local lpeg = require('lpeg')
local ffi  = require('ffi')
local util = require('blaze.lang.util')
local tree = require('blaze.lang.tree')
local re   = require('blaze.lang.re')
lpeg.setmaxstack(1024)

local line = 1
local defs = { }

defs['nil'] = function()
   return nil
end

function defs.chunk(line, body)
   local node = tree.ChunkNode.new(body)
   node:set_line(line)
   return node
end

function defs.stmt(line, node)
   node:set_line(line)
   return node
end

function defs.term(line, node, tail)
   node:set_line(line)
   if node.tag == 'Identifier' then
      node:set_lookup(true)
   end
   for i=1, #tail, 2 do
      local oper = tail[i]
      local expr = tail[i + 1]
      if oper == '.' then
         node = defs.memberExpr(node, expr, false)
      elseif oper == '::' then
         node = defs.memberExpr(node, expr, false)
         node.namespace = true
      elseif oper == '::[' then
         node = defs.memberExpr(node, expr, true)
         node.namespace = true
      elseif oper == '[' then
         node = defs.memberExpr(node, expr, true)
      elseif oper == '(' then
         node = defs.callExpr(node, expr)
      else
         error("PANIC: parser is broken")
      end
   end
   return node
end

function defs.newExpr(base, types, args)
   return tree.NewExpression.new(base, types, args)
end

function defs.expr(line, node)
   node:set_line(line)
   return node
end

function defs.declStmt(deco, node)
   node:set_decorators(decos)
   return node
end

function defs.decorator(term)
   return tree.DecoratorNode.new(term)
end

function defs.includeStmt(line, part)
   local node = tree.IncludeStatement.new(part)
   node:set_line(line)
   return node
end

function defs.traitDecl(name, base, body)
   return tree.TraitNode.new(name, base, body)
end

function defs.richString(exprs)
   for i=1, #exprs do
      if type(exprs[i]) == "string" then
         exprs[i] = defs.literal(exprs[i])
      end
   end
   return tree.RichString.new(exprs)
end

function defs.moduleStmt(path)
   return tree.ModuleStatement.new(path)
end

function defs.importStmt(names, from)
   return tree.ImportStatement.new(names, from)
end

function defs.importTerm(alias, name)
   if not name then
      name = alias
   end
   return tree.ImportTerm.new(name, alias)
end

function defs.exportStmt(names)
   for i=1, #names do
      names[i]:set_lookup(true)
   end
   return tree.ExportStatement.new(names)
end

function defs.literal(value)
   return tree.Literal.new(value)
end

function defs.literalNumber(s)
   return tree.Literal.new(tonumber(s))
end

function defs.boolean(val)
   return val == 'true'
end

function defs.nilExpr()
   return tree.Literal.new(nil)
end

function defs.identifier(name)
   return tree.Identifier.new(name)
end

function defs.typeExpr(expr)
   return expr
end

function defs.typeParams(list)
   return tree.TypeParams.new(list)
end

function defs.typeVariance(name, base)
   return tree.TypeVariance.new(name, base)
end

function defs.typeName(base, args)
   return tree.TypeName.new(base, args)
end

function defs.funcType(params, returns)
   return tree.FunctionType.new(params, returns)
end

function defs.typeList(list)
   return tree.TypeList.new(list)
end

function defs.typeUnion(exprs)
   if #exprs == 1 then
      return exprs[1]
   end
   local a = exprs[1]
   for i=2, #exprs do
      a = tree.TypeUnion.new(a, exprs[i])
   end
   return a
end

function defs.typedIdent(ident, texpr)
   ident.type = texpr
   return ident
end

function defs.compExpr(body, blocks)
   return tree.ArrayComprehension.new(body, blocks)
end

function defs.compBlock(left, right, filter)
   return tree.ComprehensionBlock.new(left, right, filter)
end

function defs.arrayExpr(elements)
   return tree.ArrayLiteral.new(elements)
end

function defs.arrayPatt(elements)
   return tree.ArrayPattern.new(elements)
end

function defs.tablePatt(entries, coerce)
   return tree.TablePattern.new(entries, coerce)
end

function defs.tableItem(item)
   return tree.TableItem.new(item)
end

function defs.tablePair(pair)
   return tree.TablePair.new(pair)
end

function defs.tableExpr(entries)
   return tree.TableLiteral.new(entries)
end

function defs.whileStmt(test, body)
   return tree.WhileStatement.new(test, body)
end

function defs.ifStmt(test, cons, altn)
   if cons.tag ~= "BlockNode" then
      cons = defs.block{ cons }
   end
   if altn and altn.tag ~= "BlockNode" then
      altn = defs.block{ altn }
   end
   return tree.IfStatement.new(test, cons, altn)
end

function defs.blockExpr(node)
   return defs.callExpr(
      defs.funcExpr({ }, defs.block{ node }),
      { }
   )
end

function defs.loopExpr(node)
   local wrap = defs.memberExpr(
      defs.identifier("coroutine"),
      defs.identifier("wrap")
   )
   wrap:set_namespace(true)
   return defs.callExpr(
      wrap, { defs.funcExpr({ }, defs.block{ node }) }
   )
end

function defs.repeatStmt(body, test)
   return tree.RepeatStatement.new(test, body)
end

function defs.forStmt(name, init, last, step, body)
   return tree.ForStatement.new(name, init, last, step, body)
end

function defs.forInStmt(left, right, body)
   return tree.ForInStatement.new(left, right, body)
end

function defs.spreadExpr(argument)
   return tree.SpreadExpression.new(argument)
end

function defs.param(cap)
   return tree.ParameterNode.new(cap.name, cap.type, cap.init, cap.rest)
end

function defs.paramList(params)
   return tree.ParameterList.new(params)
end

function defs.funcHead(params, returns)
   return tree.SignatureNode.new(params, returns)
end

function defs.funcDecl(name, head, body)
   if body.tag ~= "BlockNode" then
      body = defs.block{ defs.returnStmt(body) }
   end
   return tree.FunctionNode.new(name, head, body)
end

function defs.localFuncDecl(name, head, body)
   local decl = defs.funcDecl(name, head, body)
   decl:set_local(true)
   return decl
end

function defs.localCoroDecl(name, head, body)
   local decl = defs.funcDecl(name, head, body)
   decl:set_local(true)
   decl:set_generator(true)
   return decl
end

function defs.funcExpr(head, body)
   local decl = defs.funcDecl(nil, head, body)
   decl:set_expression(true)
   return decl
end

function defs.coroExpr(...)
   local expr = defs.funcExpr(...)
   expr:set_generator(true)
   return expr
end

function defs.coroDecl(...)
   local decl = defs.funcDecl(...)
   decl:set_generator(true)
   return decl
end

function defs.coroMeth(...)
   local prop = defs.methDecl(...)
   prop:set_generator(true)
   return prop
end

function defs.macroDecl(name, head, body)
   return tree.MacroNode.new(name, head, body)
end

function defs.block(body)
   return tree.BlockNode.new(body)
end

function defs.givenStmt(disc, cases, default)
   if default then
      cases[#cases + 1] = tree.GivenCase.new(nil, nil, default)
   end
   return tree.GivenStatement.new(disc, cases)
end

function defs.givenCase(test, guard, cons)
   if test and test.tag == 'CallExpression' then
      test = setmetatable(test, tree.ApplyPattern)
   end
   return tree.GivenCase.new(test, guard[1], cons)
end

function defs.takeStmt(args)
   return tree.TakeStatement.new(args)
end

function defs.returnStmt(args)
   return tree.ReturnStatement.new(args)
end

function defs.labelStmt(label)
   return tree.Label.new(label)
end

function defs.gotoStmt(label)
   return tree.GotoStatement.new(label)
end

function defs.breakStmt()
   return tree.BreakStatement.new()
end

function defs.continueStmt()
   return tree.ContinueStatement.new()
end

function defs.throwStmt(expr)
   return tree.ThrowStatement.new(expr)
end

function defs.tryStmt(body, handlers, finalizer)
   return tree.TryStatement.new(body, handlers, finalizer)
end

function defs.catchClause(param, guard, body)
   if not body then
      body, guard = guard, nil
   end
   return tree.CatchClause.new(param, guard, body)
end

function defs.classDecl(name, args, base, body)
   return tree.ClassNode.new(name, args, base, body)
end

function defs.classMember(deco, node)
   node:set_decorators(deco)
   return node
end

function defs.propDecl(cap)
   return tree.PropertyNode.new(cap.name, cap.type, cap.init)
end

function defs.methDecl(kind, name, head, body)
   local node = tree.MethodNode.new(name, head, body)
   if kind == "get" then
      node:set_getter(true)
   elseif kind == "set" then
      node:set_setter(true)
   end
   return node
end

function defs.exprStmt(line, expr)
   local node = tree.ExpressionStatement.new(expr)
   node:set_line(line)
   return node
end

function defs.selfExpr()
   return tree.SelfExpression.new()
end

function defs.superExpr()
   return tree.SuperExpression.new()
end

function defs.prefixExpr(left, term)
   if left and term then
      return tree.UnaryExpression.new(left, term)
   end
   return left
end

function defs.parenExpr(expr)
   return tree.ParenExpression.new(expr)
end

function defs.memberExpr(object, property, computed)
   return tree.MemberExpression.new(object, property, computed)
end

function defs.callExpr(expr, args)
   return tree.CallExpression.new(expr, args)
end

function defs.binaryExpr(op, lhs, rhs)
   return tree.BinaryExpression.new(op, lhs, rhs)
end

function defs.logicalExpr(op, lhs, rhs)
   return tree.LogicalExpression.new(op, lhs, rhs)
end

function defs.assignExpr(lhs, oper, rhs)
   for i=1, #lhs do
      if lhs[i].tag == 'CallExpression' then
         setmetatable(lhs[i], tree.ApplyPattern)
      end
   end
   return tree.AssignExpression.new(oper, lhs, rhs)
end

function defs.updateExpr(lhs, rhs)
   if rhs then
      if rhs.oper == '=' then
         return defs.assignExpr({ lhs, unpack(rhs) }, '=', rhs.list)
      elseif rhs.oper == 'in' then
         lhs = { lhs, unpack(rhs) }
         rhs = rhs.list[1]
         return defs.assignExpr(lhs, 'in', { defs.inExpr(lhs, rhs) })
      else
         return tree.UpdateExpression.new(rhs.oper, lhs, rhs.expr)
      end
   else
      return lhs
   end
end


local valid_lhs = {
   Identifier   = true,
   ArrayPattern = true,
   ApplyPattern = true,
   TablePattern = true
}

function defs.localDecl(name, lhs, oper, rhs)
   for i=1, #lhs do
      if lhs[i].tag == 'CallExpression' then
         setmetatable(lhs[i], tree.ApplyPattern)
      elseif not valid_lhs[lhs[i].tag] then
         print(string.format(
            "Error: %s:%s: invalid left hand side in local declaration",
            tostring(name), tostring(line)
         ))
         os.exit(1)
      end
   end
   if oper == 'in' then
      rhs = { defs.inExpr(lhs, rhs) }
   end

   return tree.LocalDeclaration.new(lhs, rhs)
end

function defs.inExpr(names, expr)
   return tree.InExpression.new(names, expr)
end

function defs.doStmt(block)
   return tree.DoStatement.new(block)
end

local op_info = {
   ["or"]  = { 1, 'L' },
   ["and"] = { 2, 'L' },

   ["=="]  = { 3, 'L' },
   ["!="]  = { 3, 'L' },
   ["~~"]  = { 3, 'L' },
   ["!~"]  = { 3, 'L' },

   ["is"]  = { 4, 'L' },
   ["as"]  = { 4, 'L' },

   ["with"]= { 4, 'R' },

   [">="]  = { 5, 'L' },
   ["<="]  = { 5, 'L' },
   [">"]   = { 5, 'L' },
   ["<"]   = { 5, 'L' },

   ["|"]   = { 6, 'L' },
   ["^"]   = { 7, 'L' },
   ["&"]   = { 8, 'L' },

   ["<<"]  = { 11, 'L' },
   [">>"]  = { 11, 'L' },
   [">>>"] = { 11, 'L' },

   ["~"]   = { 12, 'L' },
   ["+"]   = { 12, 'L' },
   ["-"]   = { 12, 'L' },
   [".."]  = { 12, 'R' },

   ["*"]   = { 13, 'L' },
   ["/"]   = { 13, 'L' },
   ["%"]   = { 13, 'L' },

   ["~_"]  = { 14, 'R' },
   ["-_"]  = { 14, 'R' },
   ["+_"]  = { 14, 'R' },
   ["!_"]  = { 14, 'R' },

   ["not_"] = { 14, 'R' },

   ["**"]  = { 15, 'R' },
   ["#_"]  = { 16, 'R' },

   -- user operators
   [":!"]  = { 3, 'L' },
   [":?"]  = { 3, 'L' },
   [":="]  = { 5, 'L' },
   [":>"]  = { 5, 'L' },
   [":<"]  = { 5, 'L' },
   [":|"]  = { 6, 'L' },
   [":^"]  = { 7, 'L' },
   [":&"]  = { 8, 'L' },
   [":~"]  = { 12, 'L' },
   [":+"]  = { 12, 'L' },
   [":-"]  = { 12, 'L' },
   [":*"]  = { 13, 'L' },
   [":/"]  = { 13, 'L' },
   [":%"]  = { 13, 'L' },
}

local patt_op_info = {
   ["~>"]  = { 1, 'L' },
   ["->"]  = { 1, 'L' },
   ["+>"]  = { 1, 'L' },

   ["|"]   = { 2, 'L' },

   ["&_"]  = { 3, 'R' },
   ["!_"]  = { 3, 'R' },

   ["+"]   = { 3, 'L' },
   ["*"]   = { 3, 'L' },
   ["?"]   = { 3, 'L' },

   ["^+"]  = { 4, 'R' },
   ["^-"]  = { 4, 'R' },
}

local function debug(t)
   return (string.gsub(util.dump(t), "%s+", " "))
end

local shift = table.remove
local fold_expr, fold_atom

function fold_atom(exp)
   local lhs = shift(exp, 1)
   if type(lhs) == 'table' and lhs.tag == 'ParenExpression' then
      return fold_expr({lhs.expression}, 1)
   end
   return lhs
end

function fold_expr(exp, min)
   local lhs = fold_atom(exp, 1)
   if type(lhs) == 'table' and lhs.tag == 'UnaryExpression' then
      local op   = lhs.operator..'_'
      local info = op_info[op]
      table.insert(exp, 1, lhs.argument)
      lhs.argument = fold_expr(exp, info[1])
   end
   while op_info[exp[1]] ~= nil and op_info[exp[1]][1] >= min do
      local op = shift(exp, 1)
      local info = op_info[op]
      local prec, assoc = info[1], info[2]
      if assoc == 'L' then
         prec = prec + 1
      end
      local rhs = fold_expr(exp, prec)
      if op == "or" or op == "and" then
         lhs = defs.logicalExpr(op, lhs, rhs)
      else
         lhs = defs.binaryExpr(op, lhs, rhs)
      end
   end
   return lhs
end

function defs.infixExpr(expr)
   if expr[2] then
      return fold_expr(expr, 0)
   end
   return expr[1]
end

function defs.pattLiteral(expr)
   return tree.PatternLiteral.new(expr)
end

function defs.grammarDecl(name, base, body)
   return tree.GrammarNode.new(name, base, body)
end

function defs.pattRule(name, body)
   return tree.PatternRule.new(name, body)
end

function defs.pattGrammar(rules)
   return tree.PatternGrammar.new(rules)
end

function defs.pattExpr(line, expr)
   if expr == nil then
      expr = defs.pattTerm("")
   end
   expr:set_line(line)
   return expr
end

function defs.pattAlt(list)
   return util.fold_left(list, function(a, b)
      return tree.PatternAlternate.new(a, b)
   end)
end

function defs.pattSeq(list)
   return util.fold_left(list, function(a, b)
      return tree.PatternSequence.new(a, b)
   end)
end

function defs.pattAny()
   return tree.PatternAny.new()
end

function defs.pattAssert(oper, term)
   return tree.PatternAssert.new(oper, term)
end

function defs.pattSuffix(term, tail)
   if #tail == 0 then
      return term
   end
   local left = term
   for i=1, #tail do
      tail[i].left = left
      left = tail[i]
   end
   return left
end

function defs.pattAct(oper, expr)
   return tree.PatternAction.new(oper, expr)
end

function defs.pattOpt(oper)
   local count
   if oper == '?' then
      count = -1
   elseif oper == '*' then
      count = 0
   else assert(oper == '+')
      count = 1
   end
   return tree.PatternRepeat.new(count)
end

function defs.pattRep(count)
   return tree.PatternRepeat.new(tonumber(count))
end

function defs.pattCaptSubst(expr)
   return { tag = "PatternCaptSubst", pattern = expr }
end

function defs.pattCaptTable(expr)
   return tree.PatternCaptTable.new(expr or defs.literal(""))
end

function defs.pattCaptBasic(expr)
   return tree.PatternCaptBasic.new(expr)
end

function defs.pattCaptConst(expr)
   return tree.PatternCaptConst.new(expr)
end

function defs.pattCaptGroup(name, expr)
   return tree.PatternCaptGroup.new(name, expr)
end

function defs.pattCaptBack(name)
   return tree.PatternCaptBack.new(name)
end

function defs.pattCaptBackRef(name)
   return tree.PatternCaptBackRef.new(name)
end

function defs.pattRef(name)
   return tree.PatternReference.new(name)
end

function defs.pattClass(prefix, items)
   local expr = util.fold_left(items, function(a, b)
      return tree.PatternAlternate.new(a, b)
   end)
   return tree.PatternClass.new(expr, prefix == "^")
end

function defs.pattRange(left, right)
   return tree.PatternRange.new(left, right)
end

function defs.pattName(name)
   return name
end

function defs.pattTerm(literal)
   return tree.PatternTerm.new(literal)
end

function defs.pattPredef(name)
   return tree.PatternPredef.new(name)
end

function defs.pattArg(index)
   return tree.PatternArgument.new(index)
end

function defs.incline()
   line = line + 1
end

function defs.curline()
   return line
end

function defs.setline(ln)
   local prev = line
   line = tonumber(ln) or 1
   return prev
end

function defs.topline(src, ofs, cap)
   defs.setline(tonumber(cap))
   return true, tonumber(cap)
end

local int64_t  = ffi.typeof('int64_t')
local uint64_t = ffi.typeof('uint64_t')
function defs.integer(n, c)
   local b = 10
   if string.sub(n, 1, 2) == '0x' then
      b = 16
      n = string.sub(n, 3)
   elseif string.sub(n, 1, 2) == '0o' then
      b = 8
      n = string.sub(n, 3)
   end
   if c == 'LL' then
      return int64_t(tonumber(n, b))
   elseif c == 'ULL' then
      return uint64_t(tonumber(n, b))
   else
      return tonumber(n, b)
   end
end

function defs.double(n)
   return tonumber(n)
end

function defs.quote(s)
   return string.format("%q", s)
end

local escape_lookup = {
   ["a"] = "\a",
   ["b"] = "\b",
   ["f"] = "\f",
   ["n"] = "\n",
   ["r"] = "\r",
   ["t"] = "\t",
   ["v"] = "\v",
   ["0"] = "\0",
   ['"'] = '"',
   ["'"] = "'",
   ["\\"]= "\\"
}

function defs.escape(s)
   local t = string.sub(s, 2)
   if escape_lookup[t] then return escape_lookup[t] end
   if string.match(s, '\\%d%d?%d?') then
      return string.char(tonumber(string.sub(s, 2)))
   end
   if string.match(s, '\\x%x%x') then
      local c = '0'..string.sub(s, 2)
      return string.char(tonumber(c))
   end
   if string.sub(s, 1, 2) == '\\u%x%x%x%x' then
      local c = '0x'..string.sub(s, 3)
      return utf8.char(tonumber(c))
   end
   error(string.format("invalid escape sequence: %q on line %s", s, line))
end

function defs.error(src, pos, name)
   local loc = string.sub(src, pos, pos)
   if loc == '' then
      error("Unexpected end of input", 2)
   else
      local tok = string.match(src, '%s*(%S+)', pos) or loc
      local ln  = 1
      local ofs = 0
      while ofs < pos do
         local a, b = string.find(src, "\n", ofs)
         if a then
            ofs = a + 1
            if ofs < pos then
              ln = ln + 1
            end
         else
            break
         end
      end
      error("Unexpected token '"..tok.."' on line "..tostring(ln).." "..tostring(name or '?'), 2)
   end
end

function defs.fail(src, pos, msg)
   local loc = string.sub(src, pos, pos)
   if loc == '' then
      error("Unexpected end of input")
   else
      local tok = string.match(src, '(%w+)', pos) or loc
      error(msg.." near '"..tok.."'")
   end
end

local patt = [=[
   chunk  <- ((%2 => topline) {|
      <shebang>? s
      (<module_decl> s)?
      (<include_stmt> (<sep> s <include_stmt>)* <sep>? s)?
      (<stmt> (<sep> s <stmt>)* <sep>?)? s (!. / %1 => error)
   |}) -> chunk

   shebang  <- '#!' (!<nl> .)*

   lcomment <- "//" (!<nl> .)* <nl>

   bcomment <- '/*' (<nl> / (!'*/' .))* '*/'

   comment  <- <bcomment> / <lcomment>

   idsafe   <- !(%alnum / "_" / "!" / "?" / "$")
   nl       <- %nl -> incline
   s        <- (<comment> / <nl> / !%nl %s)*
   S        <- (<comment> / <nl> / !%nl %s)+
   ws       <- <nl> / %s
   hs       <- (!%nl %s)*
   HS       <- (!%nl %s)+
   word     <- (%alpha / "_" / "$" / "?") (%alnum / "_" / "$" / "!" / "?")*

   reserved <- (
      "let" / "function" / "nil" / "true" / "false" / "return"
      / "break" / "goto" / "do" / "for" / "in" / "take" / "has"
      / "while" / "repeat" / "until" / "if" / "else"
   ) <idsafe>

   keyword  <- (
      <reserved> / "class" / "module" / "continue" / "super"
      / "import" / "export" / "is" / "as"
      / "include" / "grammar" / "given" / "case" / "with"
   ) <idsafe>

   sep <- <bcomment>? (<nl> / ";" / <lcomment>) / <ws> <sep>?

   escape <- {~ ('\' (
      'x' %xdigit %xdigit / 'u' %xdigit %xdigit %xdigit %xdigit
      / %digit (%digit %digit?)? / .
   )) -> escape ~}

   astring <- (
      "'''" {~ (<nl> / ("\\" -> "\") / ("\'" -> "'") / {!"'''" .})* ~} "'''"
   ) / (
      "'" {~ (<nl> / ("\\" -> "\") / ("\'" -> "'") / {!"'" .})* ~} "'"
   )

   qstring <- {|
      ('"""' (
         <interp_expr> / {~ (<escape> / <nl> / !(<interp_expr> / '"""') .)+ ~}
      )* '"""')
      /
      ('"' (
         <interp_expr> / {~ (<escape> / <nl> / !(<interp_expr> / '"') .)+ ~}
      )* '"')
   |} -> richString

   interp_expr <- (
      "%{" s <expr> s "}"
   )

   string <- (<astring> -> literal) / <qstring>

   octal <- { "0o" [0-7]+ }

   heximal <- { "0x" %xdigit+ }

   decexp <- ("e"/"E") "-"? %digit+

   double <- (
      %digit+ ("." !"." %digit+ <decexp>? / <decexp>)
   ) -> double

   integer <- ((
      <heximal> / <octal> / { %digit+ }
   ) {'LL' / 'ULL'}?) -> integer

   number <- <double> / <integer>

   boolean <- (
      {"true"/"false"} <idsafe>
   ) -> boolean

   literal <- ( <number> / <astring> / <boolean> ) -> literal

   in  <- "in" <idsafe>
   end <- "}"

   module_decl <- (
      "module" <idsafe> s {| <module_path> |}
   ) -> moduleStmt

   module_path <- (
      (<ident> (s "." s <module_path>)) / <ident>
   )

   export_stmt <- (
      "export" <idsafe> s {| <ident_list> |}
   ) -> exportStmt

   import_stmt <- (
      "import" <idsafe> s (<import_from> / {| |} <string>)
   ) -> importStmt

   import_from <- (
      {| <import_term> (s "," s <import_term>)* |} s
      "from" <idsafe> s <string>
   )

   import_term <- (
      <ident> (hs "=" hs <ident>)?
   ) -> importTerm

   stmt <- (('' -> curline) (
      <import_stmt>
      / <export_stmt>
      / <if_stmt>
      / <while_stmt>
      / <repeat_stmt>
      / <for_stmt>
      / <for_in_stmt>
      / <do_stmt>
      / <decl_stmt>
      / <return_stmt>
      / <take_stmt>
      / <break_stmt>
      / <continue_stmt>
      / <given_stmt>
      / <label_stmt>
      / <goto_stmt>
      / <expr_stmt>
   )) -> stmt

   stmt_list <- {|
      (<stmt> (<sep> s <stmt>)* <sep>?)?
   |}

   label_stmt <- (
      <ident> ':' !':'
   ) -> labelStmt

   goto_stmt <- (
      'goto' <idsafe> hs <ident>
   ) -> gotoStmt

   take_stmt <- (
      "take" <idsafe> {| (hs <expr_list>)? |}
   ) -> takeStmt

   break_stmt <- (
      "break" <idsafe>
   ) -> breakStmt

   continue_stmt <- (
      "continue" <idsafe>
   ) -> continueStmt

   return_stmt <- (
      "return" <idsafe> {| (hs <expr_list>)? |}
   ) -> returnStmt

   decl_stmt <- (
      {| (<decorator> (s <decorator>)* s)? |} (
           <let_func>
         / <let_decl>
         / <coro_decl>
         / <func_decl>
         / <class_decl>
         / <trait_decl>
         / <grammar_decl>
      )
   ) -> declStmt

   decorator <- (
      "@" <term>
   ) -> decorator

   typed_ident <- (
      <ident> hs ":" s <type_expr>
   ) -> typedIdent

   let_decl <- (
      "let" <idsafe> %1 s {| <bind_left> (s "," s <bind_left>)* |}
      (s ({"="} s {| <expr_list> |} / {"in"} <idsafe> s <expr>))?
   ) -> localDecl

   let_func <- (
      "let" <idsafe> s <ident> s <func_head> s <func_body>
   ) -> localFuncDecl

   bind_left <- (
        <table_patt>
      / <typed_ident>
      / <term>
   )

   table_sep <- (
      hs (","/";"/<nl>)
   )

   table_patt <- (
      "[" s {|
         <table_patt_pair> (<table_sep> s <table_patt_pair>)*
         <table_sep>?
      |} s ("]" / %1 => error)
      (hs ":" hs <expr>)?
   ) -> tablePatt

   table_patt_pair <- {|
      ({:name: <name> :}
         / {:expr: "[" s <expr> s ("]" / %1 -> error) :}
      ) s "=" s {:value: <bind_left> :}
      / {:value: <bind_left> :}
   |}

   ident_list <- (
      <ident> (s "," s <ident>)*
   )

   expr_list <- (
      <expr> (s "," s <expr>)*
   )

   func_decl <- (
      <ident> <func_head>
      (hs "=>" s {| <expr_list> |} / s <func_body>)
   ) -> funcDecl

   func_head <- (
      "(" s <param_list> s ")" (hs ":" <type_list> / '' -> nil)
   ) -> funcHead

   func_expr <- (
      <func_head> (hs "=>" s {| <expr_list> |} / s <func_body>)
   ) -> funcExpr

   func_body <- <block>

   coro_expr <- (
      "*" <func_head> (hs "=>" s {| <expr_list> |} / s <func_body>)
   ) -> coroExpr

   coro_decl <- (
      "*" <ident> s <func_head> s <func_body>
   ) -> coroDecl

   include_stmt <- (
      "include" <idsafe> s {| <expr_list> |}
   ) -> includeStmt

   trait_decl <- (
      "trait" <idsafe> s <ident>
      (s "<" <type_params> ">" / '' -> nil)
      (s <class_heritage> / '' -> nil) s
      <class_body>
   ) -> traitDecl

   class_decl <- (
      "class" <idsafe> s <ident>
      (s "<" <type_params> ">" / '' -> nil)
      (s <class_heritage> / '' -> nil) s
      <class_body>
   ) -> classDecl

   class_body <- {|
      "{" s (<class_body_stmt> (<sep> s <class_body_stmt>)* <sep>?)?  s
      ("}" / %1 => error)
   |} -> block

   class_body_stmt <- (('' -> curline) (
      <class_member>
   )) -> stmt

   class_member <- (
      {| (<decorator> (s <decorator>)* s)? |} (
         <coro_decl> / <meth_decl> / <prop_decl>
      )
   ) -> classMember

   class_heritage <- (
      "extends" <idsafe> s <type_name>
   )

   prop_decl <- {|
      {:name: <ident> :}
      (s ":" s {:type: <type_expr> :} / '' -> nil)
      (s "=" s {:init: <expr> :})?
   |} -> propDecl

   meth_decl <- (
      ({"get"/"set"} <idsafe> HS &<ident> / '' -> "init") <ident> s
      <func_head> s <func_body>
   ) -> methDecl

   type_name <- (
      <ident> ("<" <type_list> ">" / '' -> nil)
   ) -> typeName

   type_term <- (
      <type_name> / <func_type>
   )

   func_type <- (
      "(" s <type_list> s ")" s "=>" s
      (<type_name> / "(" s <type_list> s ")")
   ) -> funcType

   type_list <- (
      {| s <type_expr> (s "," s <type_expr>)* |}
   ) -> typeList

   type_expr <- (
      <type_variance> / <type_union>
   ) -> typeExpr

   type_union <- (
      {| <type_term> (s "|" s <type_term>)* |}
   ) -> typeUnion

   type_params <- (
      {| <type_param> (s "," s <type_param>)* |}
   ) -> typeParams

   type_param <- (
      <type_variance> / <ident>
   )

   type_variance <- (
      <ident> s "extends" <idsafe> s <ident>
   ) -> typeVariance

   param <- {|
      {:name: <ident> :}
      (s ":" s {:type: <type_expr> :})?
      (s "=" s {:init: <expr> :})?
   |} -> param

   param_list <- {| <param_next>? |} -> paramList

   param_next <- (
        <param> s "," s <param_next>
      / <param> s "," s <param_rest>
      / <param>
      / <param_rest>
   )

   param_rest <- {| "..." {:name: <ident>? :} {:rest: '' -> 'true' :} |}

   block <- ("{" s <block_body> s ("}" / %1 => error)) -> block

   block_body <- (
      {| (<stmt> (<sep> s <stmt>)* <sep>?)? |}
   )

   if_stmt <- (
      "if" <idsafe> s <expr> s <block> (
         s "else" <idsafe> s (<if_stmt> / <block>)
      )?
   ) -> ifStmt

   given_stmt <- (
      "given" <idsafe> s <expr> s "{" s
         ({| <given_case>+ |} / %1 => error)
         (s "else" <idsafe> s <block>)? s
      ("}" / %1 => error)
   ) -> givenStmt

   given_case <- (
      s "case" <idsafe> s (
           <table_patt>
         / <expr>
      )
      {| (s "if" <idsafe> s <expr>)? |}
      s <block>
   ) -> givenCase

   for_stmt <- (
      "for" <idsafe> s <ident> s "=" s <expr> s "," s <expr>
      (s "," s <expr> / ('' -> '1') -> literalNumber) s
      <loop_body>
   ) -> forStmt

   for_in_stmt <- (
      "for" <idsafe> s {| <ident_list> |} s <in> s <expr> s
      <loop_body>
   ) -> forInStmt

   loop_body <- <block>

   do_stmt <- "do" <idsafe> s <loop_body> -> doStmt

   while_stmt <- (
      "while" <idsafe> s <expr> s <loop_body>
   ) -> whileStmt

   repeat_stmt <- (
      "repeat" <idsafe> s <block> s ("until" <idsafe> hs <expr> / %1 => error)
   ) -> repeatStmt

   ident <- (
      !<keyword> { <word> }
   ) -> identifier

   name <- (
      !<reserved> { <word> }
   ) -> identifier

   new_expr <- (
      "new" <idsafe> s <ident> ("<" <type_list> ">" / '' -> nil)
      ("(" s {| <expr_list>? |} s (")" / %1 => error))?
   ) -> newExpr

   primary <- (('' -> curline) (
        <coro_expr>
      / <func_expr>
      / <new_expr>
      / <nil_expr>
      / <super_expr>
      / <table_expr>
      / <pattern>
      / <ident>
      / <literal>
      / <qstring>
      / (("(" s <expr> s (")" / %1 => error)) -> parenExpr)
   ))

   term <- (
      <primary> {| (
         s {'.' / '::'} s <name> -- ("<" <type_list> ">")?
         / {'::['} s <expr> s (']' / %1 => error)
         / { '[' } s <expr> s (']' / %1 => error)
         / { '(' } s {| <expr_list>? |} s (")" / %1 => error)
      )* |}
   ) -> term

   expr <- (('' -> curline) (
      <do_expr>
      / <if_expr>
      / <for_expr>
      / <for_in_expr>
      / <while_expr>
      / <repeat_expr>
      / <given_expr>
      / <infix_expr>
      / <spread_expr>
   )) -> expr

   do_expr <- <do_stmt> -> blockExpr

   if_expr <- <if_stmt> -> blockExpr

   given_expr <- <given_stmt> -> blockExpr

   for_expr <- <for_stmt> -> loopExpr

   for_in_expr <- <for_in_stmt> -> loopExpr

   while_expr <- <while_stmt> -> loopExpr

   repeat_expr <- <repeat_stmt> -> loopExpr

   spread_expr <- (
      "..." <term>?
   ) -> spreadExpr

   nil_expr <- (
      "nil" <idsafe>
   ) -> nilExpr

   super_expr <- (
      "super" <idsafe>
   ) -> superExpr

   expr_stmt <- (
      ('' -> curline) (<update_expr> / <expr>)
   ) -> exprStmt

   binop <- {
      "+" / "-" / "~~" / "~" / "/" / "**" / "*" / "%" / "^"
      / "||" / "&&" / "|" / "&" / ">>>" / ">>" / ">=" / ">"
      / "<<" / "<=" / "<" / ".." / "!=" / "==" / "!~"
      / ":" [+-~/*%^|&><!?=]
      / ("is" / "as"/ "with") <idsafe>
   }

   infix_expr  <- (
      {| <prefix_expr> (hs <binop> s <prefix_expr>)* |}
   ) -> infixExpr

   prefix_expr <- (
      ({ "#" / "-" !'-' / "~" / "!" / "&" } s)? <term>
   ) -> prefixExpr

   assop <- {
      "+=" / "-=" / "~=" / "**=" / "*=" / "/=" / "%=" / "&&="
      / "|=" / "||=" / "&=" / "^=" / "<<=" / ">>>=" / ">>="
   }

   update_expr <- (
      <bind_left> {|
         (s {:oper: <assop> :} s {:expr: <expr> :})
         / ((s "," s <bind_left>)* s {:oper: {'=' !'>' / 'in' <idsafe>} :}
             s {:list: {| <expr_list> |} :})
      |}
   ) -> updateExpr

   table_expr <- (
      "[" s {| <table_entries>? |} s ("]" / %1 => error)
   ) -> tableExpr

   table_entries <- (
      <table_entry> (<table_sep> s <table_entry>)* <table_sep>?
   )

   table_entry <- (
      <table_pair> / <table_item>
   )

   table_item <- (
      {| {:value: <expr> :} |}
   ) -> tableItem

   table_pair <- {|
      ({:name: <name> :}
         / {:expr: "[" s <expr> s ("]"/ %1 -> error) :}
      ) s "=" !'>' s {:value: <expr> :}
   |} -> tablePair

   pattern <- (
      "/" s (<patt_grammar> / <patt_expr>) s ("/" / %s => error)
   ) -> pattLiteral

   grammar_decl <- (
      "grammar" <idsafe> hs <ident> (s <class_heritage>)? s
      "{" (s <grammar_body>)? s ("}" / %1 => error)
   ) -> grammarDecl

   grammar_body <- {|
      (<grammar_body_stmt> (<sep> s <grammar_body_stmt>)* <sep>?)?
   |}

   grammar_body_stmt <- (
      <patt_rule> / <class_body_stmt>
   )

   patt_expr <- (('' -> curline) <patt_alt>) -> pattExpr

   patt_grammar <- {|
      <patt_rule> (s <patt_rule>)*
   |} -> pattGrammar

   patt_rule <- (
      <patt_name> hs '<-' s "{" (s "|")? s <patt_expr> s ("}" / %1 => error)
   ) -> pattRule

   patt_sep <- '|' !']'
   patt_alt <- {|
      <patt_seq> (s <patt_sep> s <patt_seq>)*
   |} -> pattAlt

   patt_seq <- {|
      (<patt_prefix> (s <patt_prefix>)*)?
   |} -> pattSeq

   patt_any <- '.' -> pattAny

   patt_prefix <- (
      <patt_assert> / <patt_suffix>
   )

   patt_assert  <- (
      {'&' / '!' } s <patt_prefix>
   ) -> pattAssert

   patt_suffix <- (
      <patt_primary> {| (s <patt_tail>)* |}
   ) -> pattSuffix

   patt_tail <- (
      <patt_opt> / <patt_rep> / <patt_act>
   )

   patt_act <- (
        {'~>'} s <term>
      / {'->'} s <term>
      / {'+>'} s <term>
   ) -> pattAct

   patt_opt <- (
      !'+>' { [+*?] }
   ) -> pattOpt

   patt_rep <- (
      '^' { [+-]? <patt_num> }
   ) -> pattRep

   patt_capt <- (
        <patt_capt_subst>
      / <patt_capt_const>
      / <patt_capt_group>
      / <patt_capt_table>
      / <patt_capt_basic>
      / <patt_capt_back>
      / <patt_capt_bref>
   )

   patt_capt_subst <- (
      '[~' s <patt_expr> s ('~]' / %1 => error)
   ) -> pattCaptSubst

   patt_capt_group <- (
      '[:' (<patt_name> ':')? s <patt_expr> s (':]' / %1 => error)
   ) -> pattCaptGroup

   patt_capt_table <- (
      '[|' s <patt_expr> s ('|]' / %1 => error)
   ) -> pattCaptTable

   patt_capt_basic <- (
      '[' s <patt_expr> s (']' / %1 => error)
   ) -> pattCaptBasic

   patt_capt_const <- (
      '[`' s <expr> s ('`]' / %1 => error)
   ) -> pattCaptConst

   patt_capt_back <- (
      '[=' s <patt_name> s ('=]' / %1 => error)
   ) -> pattCaptBack

   patt_capt_bref <- (
      '=' <patt_name>
   ) -> pattCaptBackRef

   patt_primary  <- (
      '(' s <patt_expr> s (')' / %1 => error)
      / <patt_term>
      / <patt_class>
      / <patt_predef>
      / <patt_capt>
      / <patt_arg>
      / <patt_any>
      / <patt_ref>
      / '<{' s <expr> s ('}>' / %1 => error)
   )

   patt_ref <- (
      '<' <patt_name> ('>' / %1 => error) / <patt_name>
   ) -> pattRef

   patt_arg <- (
      '%' { <patt_num> }
   ) -> pattArg

   patt_class <- (
      '<[' {'^' / ''} {| <patt_item> (!']>' <patt_item>)* |}
      (']>' / %1 => error)
   ) -> pattClass

   patt_item <- (
      <patt_predef> / <patt_range> / ({~ <escape> / . ~} -> pattTerm)
   )

   patt_term  <- (
      '"' ({~ (<escape> / <nl> / !'"' .)+ ~})* '"'
      / "'" ({~ (<nl> / !"'" .)+ ~})* "'"
   ) -> pattTerm

   patt_range   <- ({~ <escape> / . ~} '-' {~ <escape> / !"]" . ~}) -> pattRange
   patt_name    <- { [A-Za-z_][A-Za-z0-9_]* } -> pattName
   patt_num     <- [0-9]+

   patt_predef  <- '%' <patt_name> -> pattPredef

]=]

local grammar = re.compile(patt, defs)

local Process = require("blaze.lang.process")

local Parser = { } do
   Parser.__index = Parser

   local yield = coroutine.yield

   function Parser.new(ctx)
      return setmetatable({
         ctx = ctx;
      }, Parser)
   end

   function Parser:spawn()
      return Process.new(function(input)
         for unit in input do
            unit.tree = self:parse(unit.text, unit.path)
            yield(unit)
         end
      end)
   end

   local function link_parent(node, parent)
      node.parent = parent
      for n in node:children() do
         link_parent(n, node)
      end
   end

   function Parser:parse(src, name, line, ...)
      local tree = grammar:match(src, nil, name, line or 1, ...)
      link_parent(tree, nil)
      return tree
   end
end

return Parser


