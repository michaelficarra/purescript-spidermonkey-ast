module SpiderMonkeyAST (
  read,
  unread,

  SMAST(..),
  Node(..),

  ObjectPropertyKind(..),
  VarDeclKind(..),
  ObjectProperty(..),

  AssignmentOperator(..),
  BinaryOperator(..),
  LogicalOperator(..),
  UnaryOperator(..),
  UpdateOperator(..)

  ) where


import Data.Array (map)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (Regex(..))


data VarDeclKind = Var | Let | Const
data ObjectPropertyKind = Init | Get | Set
data ObjectProperty = ObjectProperty {kind :: ObjectPropertyKind, key :: Node, value :: Node}

data AssignmentOperator
  = AssignOp | AssignOpPlus | AssignOpMinus | AssignOpMul | AssignOpDiv | AssignOpMod
  | AssignOpLeftShift | AssignOpRightShift | AssignOpUnsignedRightShift
  | AssignOpBitOr | AssignOpBitXor | AssignOpBitAnd
data BinaryOperator
  = BinaryOpEQ | BinaryOpNEQ | BinaryOpStrictEQ | BinaryOpStrictNEQ
  | BinaryOpLT | BinaryOpLTE | BinaryOpGT | BinaryOpGTE
  | BinaryOpLeftShift | BinaryOpRightShift | BinaryOpUnsignedRightShift
  | BinaryOpPlus | BinaryOpMinus | BinaryOpMul | BinaryOpDiv | BinaryOpMod
  | BinaryOpBitOr | BinaryOpBitXor | BinaryOpBitAnd
  | BinaryOpIn | BinaryOpInstanceof
data LogicalOperator = LogicalOpOr | LogicalOpAnd
data UnaryOperator
  = UnaryOpMinus | UnaryOpPlus | UnaryOpLogicalNot | UnaryOpBitNot
  | UnaryOpTypeof | UnaryOpVoid | UnaryOpDelete
data UpdateOperator = UpdateOpIncrement | UpdateOpDecrement

data Node
  = ArrayExpression {elements :: [Maybe Node]}
  | AssignmentExpression {operator :: AssignmentOperator, left :: Node, right :: Node}
  | BinaryExpression {operator :: BinaryOperator, left :: Node, right :: Node}
  | BlockStatement {body :: [Node]}
  | BreakStatement {label :: Maybe Node}
  | CallExpression {callee :: Node, arguments :: [Node]}
  | CatchClause {param :: Node, body :: Node}
  | ConditionalExpression {test :: Node, alternate :: Node, consequent :: Node}
  | ContinueStatement {label :: Maybe Node}
  | DebuggerStatement
  | DoWhileStatement {body :: Node, test :: Node}
  | EmptyStatement
  | ExpressionStatement {expression :: Node}
  | ForInStatement {left :: Node, right :: Node, body :: Node}
  | ForStatement {init :: Maybe Node, test :: Maybe Node, update :: Maybe Node, body :: Node}
  | FunctionDeclaration {id :: Node, params :: [Node], body :: Node}
  | FunctionExpression {id :: Maybe Node, params :: [Node], body :: Node}
  | Identifier {name :: String}
  | IfStatement {test :: Node, consequent :: Node, alternate :: Maybe Node}
  | LabeledStatement {label :: Node, body :: Node}
  | LiteralBoolean {value :: Boolean}
  | LiteralNull
  | LiteralNumber {value :: Number}
  | LiteralRegExp {value :: Regex}
  | LiteralString {value :: String}
  | LogicalExpression {operator :: LogicalOperator, left :: Node, right :: Node}
  | MemberExpression {object :: Node, property :: Node, computed :: Boolean}
  | NewExpression {callee :: Node, arguments :: [Node]}
  | ObjectExpression {properties :: [ObjectProperty]}
  | Program {body :: [Node]}
  | ReturnStatement {argument :: Maybe Node}
  | SequenceExpression {expressions :: [Node]}
  | SwitchCase {test :: Maybe Node, consequent :: [Node]}
  | SwitchStatement {discriminant :: Node, cases :: [Node]}
  | ThisExpression
  | ThrowStatement {argument :: Node}
  | TryStatement {block :: Node, handler :: Maybe Node, finalizer :: Maybe Node}
  | UnaryExpression {operator :: UnaryOperator, argument :: Node}
  | UpdateExpression {operator :: UpdateOperator, argument :: Node, prefix :: Boolean}
  | VariableDeclaration {kind :: VarDeclKind, declarations :: [Node]}
  | VariableDeclarator {id :: Node, init :: Maybe Node}
  | WhileStatement {test :: Node, body :: Node}
  | WithStatement {object :: Node, body :: Node}


foreign import data SMAST :: *


readVarDeclKind :: String -> VarDeclKind
readVarDeclKind "var" = Var
readVarDeclKind "let" = Let
readVarDeclKind "const" = Const

readObjectPropertyKind :: String -> ObjectPropertyKind
readObjectPropertyKind "init" = Init
readObjectPropertyKind "get" = Get
readObjectPropertyKind "set" = Set

foreign import readObjectPropertyP "function readObjectPropertyP(node) { return node; }" :: SMAST -> {kind :: String, key :: SMAST, value :: SMAST}
readObjectProperty :: SMAST -> ObjectProperty
readObjectProperty x = ObjectProperty {kind: readObjectPropertyKind(node.kind), key: read node.key, value: read node.value}
  where node = readObjectPropertyP x

readAssignmentOperator :: String -> AssignmentOperator
readAssignmentOperator "=" = AssignOp
readAssignmentOperator "+=" = AssignOpPlus
readAssignmentOperator "-=" = AssignOpMinus
readAssignmentOperator "*=" = AssignOpMul
readAssignmentOperator "/=" = AssignOpDiv
readAssignmentOperator "%=" = AssignOpMod
readAssignmentOperator "<<=" = AssignOpLeftShift
readAssignmentOperator ">>=" = AssignOpRightShift
readAssignmentOperator ">>>=" = AssignOpUnsignedRightShift
readAssignmentOperator "|=" = AssignOpBitOr
readAssignmentOperator "^=" = AssignOpBitXor
readAssignmentOperator "&=" = AssignOpBitAnd

readBinaryOperator "==" = BinaryOpEQ
readBinaryOperator "!=" = BinaryOpNEQ
readBinaryOperator "===" = BinaryOpStrictEQ
readBinaryOperator "!==" = BinaryOpStrictNEQ
readBinaryOperator "<" = BinaryOpLT
readBinaryOperator "<=" = BinaryOpLTE
readBinaryOperator ">" = BinaryOpGT
readBinaryOperator ">=" = BinaryOpGTE
readBinaryOperator "<<" = BinaryOpLeftShift
readBinaryOperator ">>" = BinaryOpRightShift
readBinaryOperator ">>>" = BinaryOpUnsignedRightShift
readBinaryOperator "+" = BinaryOpPlus
readBinaryOperator "-" = BinaryOpMinus
readBinaryOperator "*" = BinaryOpMul
readBinaryOperator "/" = BinaryOpDiv
readBinaryOperator "%" = BinaryOpMod
readBinaryOperator "|" = BinaryOpBitOr
readBinaryOperator "^" = BinaryOpBitXor
readBinaryOperator "&" = BinaryOpBitAnd
readBinaryOperator "in" = BinaryOpIn
readBinaryOperator "instanceof" = BinaryOpInstanceof

readLogicalOperator "||" = LogicalOpOr
readLogicalOperator "&&" = LogicalOpAnd

readUnaryOperator "-" = UnaryOpMinus
readUnaryOperator "+" = UnaryOpPlus
readUnaryOperator "!" = UnaryOpLogicalNot
readUnaryOperator "~" = UnaryOpBitNot
readUnaryOperator "typeof" = UnaryOpTypeof
readUnaryOperator "void" = UnaryOpVoid
readUnaryOperator "delete" = UnaryOpDelete

readUpdateOperator "++" = UpdateOpIncrement
readUpdateOperator "--" = UpdateOpDecrement

foreign import readP
  "function readP(Nothing) {\n\
  \  return function (Just) {\n\
  \  return function recurse(node) {\n\
  \  switch(node.type) {\n\
  \  case 'ArrayExpression': return ArrayExpression({elements: [].map.call(node.elements, function(e){ return e == null ? Nothing : Just(recurse(e)); })});\n\
  \  case 'AssignmentExpression': return AssignmentExpression({operator: readAssignmentOperator(node.operator), left: recurse(node.left), right: recurse(node.right)});\n\
  \  case 'BinaryExpression': return BinaryExpression({operator: readBinaryOperator(node.operator), left: recurse(node.left), right: recurse(node.right)});\n\
  \  case 'BlockStatement': return BlockStatement({body: [].map.call(node.body, recurse)});\n\
  \  case 'BreakStatement': return BreakStatement({label: node.label == null ? Nothing : Just(recurse(node.label))});\n\
  \  case 'CallExpression': return CallExpression({callee: recurse(node.callee), arguments: [].map.call(node.arguments, recurse)});\n\
  \  case 'CatchClause': return CatchClause({param: recurse(node.param), body: recurse(node.body)});\n\
  \  case 'ConditionalExpression': return ConditionalExpression({test: recurse(node.test), alternate: recurse(node.alternate), consequent: recurse(node.consequent)});\n\
  \  case 'ContinueStatement': return ContinueStatement({label: node.label == null ? Nothing : Just(recurse(node.label))});\n\
  \  case 'DebuggerStatement': return DebuggerStatement;\n\
  \  case 'DoWhileStatement': return DoWhileStatement({body: recurse(node.body), test: recurse(node.test)});\n\
  \  case 'EmptyStatement': return EmptyStatement;\n\
  \  case 'ExpressionStatement': return ExpressionStatement({expression: recurse(node.expression)});\n\
  \  case 'ForInStatement': return ForInStatement({left: recurse(node.left), right: recurse(node.right), body: recurse(node.body)});\n\
  \  case 'ForStatement': return ForStatement({init: node.init == null ? Nothing : Just(recurse(node.init)), test: node.test == null ? Nothing : Just(recurse(node.test)), update: node.update == null ? Nothing : Just(recurse(node.update)), body: recurse(node.body)});\n\
  \  case 'FunctionDeclaration': return FunctionDeclaration({id: recurse(node.id), params: [].map.call(node.params, recurse), body: recurse(node.body)});\n\
  \  case 'FunctionExpression': return FunctionExpression({id: node.id == null ? Nothing : Just(recurse(node.id)), params: [].map.call(node.params, recurse), body: recurse(node.body)});\n\
  \  case 'Identifier': return Identifier({name: node.name});\n\
  \  case 'IfStatement': return IfStatement({test: recurse(node.test), consequent: recurse(node.consequent), alternate: node.alternate == null ? Nothing : Just(recurse(node.alternate))});\n\
  \  case 'LabeledStatement': return LabeledStatement({label: recurse(node.label), body: recurse(node.body)});\n\
  \  case 'Literal':\n\
  \    switch({}.toString.call(node.value)) {\n\
  \      case '[object Boolean]': return LiteralBoolean({value: node.value});\n\
  \      case '[object Null]': return LiteralNull;\n\
  \      case '[object Number]': return LiteralNumber({value: node.value});\n\
  \      case '[object RegExp]': return LiteralRegExp({value: node.value});\n\
  \      case '[object String]': return LiteralString({value: node.value});\n\
  \    }\n\
  \  case 'LogicalExpression': return LogicalExpression({operator: readLogicalOperator(node.operator), left: recurse(node.left), right: recurse(node.right)});\n\
  \  case 'MemberExpression': return MemberExpression({object: recurse(node.object), property: recurse(node.property), computed: !!node.computed});\n\
  \  case 'NewExpression': return NewExpression({callee: recurse(node.callee), arguments: [].map.call(node.arguments, recurse)});\n\
  \  case 'ObjectExpression': return ObjectExpression({properties: [].map.call(node.properties, readObjectProperty)});\n\
  \  case 'Program': return Program({body: [].map.call(node.body, recurse)});\n\
  \  case 'ReturnStatement': return ReturnStatement({argument: node.argument == null ? Nothing : Just(recurse(node.argument))});\n\
  \  case 'SequenceExpression': return SequenceExpression({expressions: [].map.call(node.expressions, recurse)});\n\
  \  case 'SwitchCase': return SwitchCase({test: node.test == null ? Nothing : Just(recurse(node.test)), consequent: [].map.call(node.consequent, recurse)});\n\
  \  case 'SwitchStatement': return SwitchStatement({discriminant: recurse(node.discriminant), cases: [].map.call(node.cases, recurse)});\n\
  \  case 'ThisExpression': return ThisExpression;\n\
  \  case 'ThrowStatement': return ThrowStatement({argument: recurse(node.argument)});\n\
  \  case 'TryStatement': return TryStatement({block: recurse(node.block), handler: node.handler == null ? Nothing : Just(recurse(node.handler)), finalizer: node.finalizer == null ? Nothing : Just(recurse(node.finalizer))});\n\
  \  case 'UnaryExpression': return UnaryExpression({operator: readUnaryOperator(node.operator), argument: recurse(node.argument)});\n\
  \  case 'UpdateExpression': return UpdateExpression({operator: readUpdateOperator(node.operator), argument: recurse(node.argument), prefix: node.prefix});\n\
  \  case 'VariableDeclaration': return VariableDeclaration({kind: readVarDeclKind(node.kind), declarations: [].map.call(node.declarations, recurse)});\n\
  \  case 'VariableDeclarator': return VariableDeclarator({id: recurse(node.id), init: node.init == null ? Nothing : Just(recurse(node.init))});\n\
  \  case 'WhileStatement': return WhileStatement({test: recurse(node.test), body: recurse(node.body)});\n\
  \  case 'WithStatement': return WithStatement({object: recurse(node.object), body: recurse(node.body)});\n\
  \  }\n\
  \  throw new TypeError('Unrecognised node type: ' + JSON.stringify(node.type));\n\
  \}}}" :: forall a. Maybe a -> (a -> Maybe a) -> SMAST -> Node

read :: SMAST -> Node
read = readP Nothing Just


foreign import unreadNull "var unreadNull = null;" :: SMAST
unreadMaybe :: Maybe Node -> SMAST
unreadMaybe x = maybe unreadNull unread x

unreadVarDeclKind Var = "var"
unreadVarDeclKind Let = "let"
unreadVarDeclKind Const = "const"

unreadObjectPropertyKind Init = "init"
unreadObjectPropertyKind Get = "get"
unreadObjectPropertyKind Set = "set"

foreign import unreadObjectPropertyP "function unreadObjectPropertyP(node) { return node; }" :: {kind :: String, key :: SMAST, value :: SMAST} -> SMAST
unreadObjectProperty (ObjectProperty p) = unreadObjectPropertyP {kind: unreadObjectPropertyKind p.kind, key: unread p.key, value: unread p.value}

unreadAssignmentOperator :: AssignmentOperator -> String
unreadAssignmentOperator AssignOp = "="
unreadAssignmentOperator AssignOpPlus = "+="
unreadAssignmentOperator AssignOpMinus = "-="
unreadAssignmentOperator AssignOpMul = "*="
unreadAssignmentOperator AssignOpDiv = "/="
unreadAssignmentOperator AssignOpMod = "%="
unreadAssignmentOperator AssignOpLeftShift = "<<="
unreadAssignmentOperator AssignOpRightShift = ">>="
unreadAssignmentOperator AssignOpUnsignedRightShift = ">>>="
unreadAssignmentOperator AssignOpBitOr = "|="
unreadAssignmentOperator AssignOpBitXor = "^="
unreadAssignmentOperator AssignOpBitAnd = "&="

unreadBinaryOperator BinaryOpEQ = "=="
unreadBinaryOperator BinaryOpNEQ = "!="
unreadBinaryOperator BinaryOpStrictEQ = "==="
unreadBinaryOperator BinaryOpStrictNEQ = "!=="
unreadBinaryOperator BinaryOpLT = "<"
unreadBinaryOperator BinaryOpLTE = "<="
unreadBinaryOperator BinaryOpGT = ">"
unreadBinaryOperator BinaryOpGTE = ">="
unreadBinaryOperator BinaryOpLeftShift = "<<"
unreadBinaryOperator BinaryOpRightShift = ">>"
unreadBinaryOperator BinaryOpUnsignedRightShift = ">>>"
unreadBinaryOperator BinaryOpPlus = "+"
unreadBinaryOperator BinaryOpMinus = "-"
unreadBinaryOperator BinaryOpMul = "*"
unreadBinaryOperator BinaryOpDiv = "/"
unreadBinaryOperator BinaryOpMod = "%"
unreadBinaryOperator BinaryOpBitOr = "|"
unreadBinaryOperator BinaryOpBitXor = "^"
unreadBinaryOperator BinaryOpBitAnd = "&"
unreadBinaryOperator BinaryOpIn = "in"
unreadBinaryOperator BinaryOpInstanceof = "instanceof"

unreadLogicalOperator LogicalOpOr = "||"
unreadLogicalOperator LogicalOpAnd = "&&"

unreadUnaryOperator UnaryOpMinus = "-"
unreadUnaryOperator UnaryOpPlus = "+"
unreadUnaryOperator UnaryOpLogicalNot = "!"
unreadUnaryOperator UnaryOpBitNot = "~"
unreadUnaryOperator UnaryOpTypeof = "typeof"
unreadUnaryOperator UnaryOpVoid = "void"
unreadUnaryOperator UnaryOpDelete = "delete"

unreadUpdateOperator UpdateOpIncrement = "++"
unreadUpdateOperator UpdateOpDecrement = "--"


foreign import unreadArrayExpression
  "function unreadArrayExpression(node) {\n\
  \  return {type: 'ArrayExpression', elements: node.elements};\n\
  \}" :: {elements :: [SMAST]} -> SMAST

foreign import unreadAssignmentExpression
  "function unreadAssignmentExpression(node) {\n\
  \  return {type: 'AssignmentExpression', operator: node.operator, left: node.left, right: node.right};\n\
  \}" :: {operator :: String, left :: SMAST, right :: SMAST} -> SMAST

foreign import unreadBinaryExpression
  "function unreadBinaryExpression(node) {\n\
  \  return {type: 'BinaryExpression', operator: node.operator, left: node.left, right: node.right};\n\
  \}" :: {operator :: String, left :: SMAST, right :: SMAST} -> SMAST

foreign import unreadBlockStatement
  "function unreadBlockStatement(node) {\n\
  \  return {type: 'BlockStatement', body: node.body};\n\
  \}" :: {body :: [SMAST]} -> SMAST

foreign import unreadBreakStatement
  "function unreadBreakStatement(node) {\n\
  \  return {type: 'BreakStatement', label: node.label};\n\
  \}" :: {label :: SMAST} -> SMAST

foreign import unreadCallExpression
  "function unreadCallExpression(node) {\n\
  \  return {type: 'CallExpression', callee: node.callee, arguments: node.arguments};\n\
  \}" :: {callee :: SMAST, arguments :: [SMAST]} -> SMAST

foreign import unreadCatchClause
  "function unreadCatchClause(node) {\n\
  \  return {type: 'CatchClause', param: node.param, body: node.body};\n\
  \}" :: {param :: SMAST, body :: SMAST} -> SMAST

foreign import unreadConditionalExpression
  "function unreadConditionalExpression(node) {\n\
  \  return {type: 'ConditionalExpression', test: node.test, alternate: node.alternate, consequent: node.consequent};\n\
  \}" :: {test :: SMAST, alternate :: SMAST, consequent :: SMAST} -> SMAST

foreign import unreadContinueStatement
  "function unreadContinueStatement(node) {\n\
  \  return {type: 'ContinueStatement', label: node.label};\n\
  \}" :: {label :: SMAST} -> SMAST

foreign import unreadDebuggerStatement
  "var unreadDebuggerStatement = {type: 'DebuggerStatement'};" :: SMAST

foreign import unreadDoWhileStatement
  "function unreadDoWhileStatement(node) {\n\
  \  return {type: 'DoWhileStatement', body: node.body, test: node.test};\n\
  \}" :: {body :: SMAST, test :: SMAST} -> SMAST

foreign import unreadEmptyStatement
  "var unreadEmptyStatement = {type: 'EmptyStatement'};" :: SMAST

foreign import unreadExpressionStatement
  "function unreadExpressionStatement(node) {\n\
  \  return {type: 'ExpressionStatement', expression: node.expression};\n\
  \}" :: {expression :: SMAST} -> SMAST

foreign import unreadForInStatement
  "function unreadForInStatement(node) {\n\
  \  return {type: 'ForInStatement', left: node.left, right: node.right, body: node.body};\n\
  \}" :: {left :: SMAST, right :: SMAST, body :: SMAST} -> SMAST

foreign import unreadForStatement
  "function unreadForStatement(node) {\n\
  \  return {type: 'ForStatement', init: node.init, test: node.test, update: node.update, body: node.body};\n\
  \}" :: {init :: SMAST, test :: SMAST, update :: SMAST, body :: SMAST} -> SMAST

foreign import unreadFunctionDeclaration
  "function unreadFunctionDeclaration(node) {\n\
  \  return {type: 'FunctionDeclaration', id: node.id, params: node.params, body: node.body};\n\
  \}" :: {id :: SMAST, params :: [SMAST], body :: SMAST} -> SMAST

foreign import unreadFunctionExpression
  "function unreadFunctionExpression(node) {\n\
  \  return {type: 'FunctionExpression', id: node.id, params: node.params, body: node.body};\n\
  \}" :: {id :: SMAST, params :: [SMAST], body :: SMAST} -> SMAST

foreign import unreadIdentifier
  "function unreadIdentifier(node) {\n\
  \  return {type: 'Identifier', name: node.name};\n\
  \}" :: {name :: String} -> SMAST

foreign import unreadIfStatement
  "function unreadIfStatement(node) {\n\
  \  return {type: 'IfStatement', test: node.test, consequent: node.consequent, alternate: node.alternate};\n\
  \}" :: {test :: SMAST, consequent :: SMAST, alternate :: SMAST} -> SMAST

foreign import unreadLabeledStatement
  "function unreadLabeledStatement(node) {\n\
  \  return {type: 'LabeledStatement', label: node.label, body: node.body};\n\
  \}" :: {label :: SMAST, body :: SMAST} -> SMAST

foreign import unreadLiteralBoolean
  "function unreadLiteralBoolean(node) {\n\
  \  return {type: 'Literal', value: node.value};\n\
  \}" :: {value :: Boolean} -> SMAST
foreign import unreadLiteralNull
  "var unreadLiteralNull = {type: 'Literal', value: null};" :: SMAST
foreign import unreadLiteralNumber
  "function unreadLiteralNumber(node) {\n\
  \  return {type: 'Literal', value: node.value};\n\
  \}" :: {value :: Number} -> SMAST
foreign import unreadLiteralRegExp
  "function unreadLiteralRegExp(node) {\n\
  \  return {type: 'Literal', value: node.value};\n\
  \}" :: {value :: Regex} -> SMAST
foreign import unreadLiteralString
  "function unreadLiteralString(node) {\n\
  \  return {type: 'Literal', value: node.value};\n\
  \}" :: {value :: String} -> SMAST

foreign import unreadLogicalExpression
  "function unreadLogicalExpression(node) {\n\
  \  return {type: 'LogicalExpression', operator: node.operator, left: node.left, right: node.right};\n\
  \}" :: {operator :: String, left :: SMAST, right :: SMAST} -> SMAST

foreign import unreadMemberExpression
  "function unreadMemberExpression(node) {\n\
  \  return {type: 'MemberExpression', object: node.object, property: node.property, computed: node.computed};\n\
  \}" :: {object :: SMAST, property :: SMAST, computed :: Boolean} -> SMAST

foreign import unreadNewExpression
  "function unreadNewExpression(node) {\n\
  \  return {type: 'NewExpression', callee: node.callee, arguments: node.arguments};\n\
  \}" :: {callee :: SMAST, arguments :: [SMAST]} -> SMAST

foreign import unreadObjectExpression
  "function unreadObjectExpression(node) {\n\
  \  return {type: 'ObjectExpression', properties: node.properties};\n\
  \}" :: {properties :: [SMAST]} -> SMAST

foreign import unreadProgram
  "function unreadProgram(node) {\n\
  \  return {type: 'Program', body: node.body};\n\
  \}" :: {body :: [SMAST]} -> SMAST

foreign import unreadReturnStatement
  "function unreadReturnStatement(node) {\n\
  \  return {type: 'ReturnStatement', argument: node.argument};\n\
  \}" :: {argument :: SMAST} -> SMAST

foreign import unreadSequenceExpression
  "function unreadSequenceExpression(node) {\n\
  \  return {type: 'SequenceExpression', expressions: node.expressions};\n\
  \}" :: {expressions :: [SMAST]} -> SMAST

foreign import unreadSwitchCase
  "function unreadSwitchCase(node) {\n\
  \  return {type: 'SwitchCase', test: node.test, consequent: node.consequent};\n\
  \}" :: {test :: SMAST, consequent :: [SMAST]} -> SMAST

foreign import unreadSwitchStatement
  "function unreadSwitchStatement(node) {\n\
  \  return {type: 'SwitchStatement', discriminant: node.discriminant, cases: node.cases};\n\
  \}" :: {discriminant :: SMAST, cases :: [SMAST]} -> SMAST

foreign import unreadThisExpression
  "var unreadThisExpression = {type: 'ThisExpression'};" :: SMAST

foreign import unreadThrowStatement
  "function unreadThrowStatement(node) {\n\
  \  return {type: 'ThrowStatement', argument: node.argument};\n\
  \}" :: {argument :: SMAST} -> SMAST

foreign import unreadTryStatement
  "function unreadTryStatement(node) {\n\
  \  return {type: 'TryStatement', block: node.block, handler: node.handler, finalizer: node.finalizer};\n\
  \}" :: {block :: SMAST, handler :: SMAST, finalizer :: SMAST} -> SMAST

foreign import unreadUnaryExpression
  "function unreadUnaryExpression(node) {\n\
  \  return {type: 'UnaryExpression', operator: node.operator, argument: node.argument};\n\
  \}" :: {operator :: String, argument :: SMAST} -> SMAST

foreign import unreadUpdateExpression
  "function unreadUpdateExpression(node) {\n\
  \  return {type: 'UpdateExpression', operator: node.operator, argument: node.argument, prefix: node.prefix};\n\
  \}" :: {operator :: String, argument :: SMAST, prefix :: Boolean} -> SMAST

foreign import unreadVariableDeclaration
  "function unreadVariableDeclaration(node) {\n\
  \  return {type: 'VariableDeclaration', kind: node.kind, declarations: node.declarations};\n\
  \}" :: {kind :: String, declarations :: [SMAST]} -> SMAST

foreign import unreadVariableDeclarator
  "function unreadVariableDeclarator(node) {\n\
  \  return {type: 'VariableDeclarator', id: node.id, init: node.init};\n\
  \}" :: {id :: SMAST, init :: SMAST} -> SMAST

foreign import unreadWhileStatement
  "function unreadWhileStatement(node) {\n\
  \  return {type: 'WhileStatement', test: node.test, body: node.body};\n\
  \}" :: {test :: SMAST, body :: SMAST} -> SMAST

foreign import unreadWithStatement
  "function unreadWithStatement(node) {\n\
  \  return {type: 'WithStatement', object: node.object, body: node.body};\n\
  \}" :: {object :: SMAST, body :: SMAST} -> SMAST

unread :: Node -> SMAST
unread (ArrayExpression a) = unreadArrayExpression {elements: map unreadMaybe a.elements}
unread (AssignmentExpression a) = unreadAssignmentExpression {operator: unreadAssignmentOperator a.operator, left: unread a.left, right: unread a.right}
unread (BinaryExpression a) = unreadBinaryExpression {operator: unreadBinaryOperator a.operator, left: unread a.left, right: unread a.right}
unread (BlockStatement a) = unreadBlockStatement {body: map unread a.body}
unread (BreakStatement a) = unreadBreakStatement {label: unreadMaybe a.label}
unread (CallExpression a) = unreadCallExpression {callee: unread a.callee, arguments: map unread a.arguments}
unread (CatchClause a) = unreadCatchClause {param: unread a.param, body: unread a.body}
unread (ConditionalExpression a) = unreadConditionalExpression {test: unread a.test, alternate: unread a.alternate, consequent: unread a.consequent}
unread (ContinueStatement a) = unreadContinueStatement {label: unreadMaybe a.label}
unread DebuggerStatement = unreadDebuggerStatement
unread (DoWhileStatement a) = unreadDoWhileStatement {body: unread a.body, test: unread a.test}
unread EmptyStatement = unreadEmptyStatement
unread (ExpressionStatement a) = unreadExpressionStatement {expression: unread a.expression}
unread (ForInStatement a) = unreadForInStatement {left: unread a.left, right: unread a.right, body: unread a.body}
unread (ForStatement a) = unreadForStatement {init: unreadMaybe a.init, test: unreadMaybe a.test, update: unreadMaybe a.update, body: unread a.body}
unread (FunctionDeclaration a) = unreadFunctionDeclaration {id: unread a.id, params: map unread a.params, body: unread a.body}
unread (FunctionExpression a) = unreadFunctionExpression {id: unreadMaybe a.id, params: map unread a.params, body: unread a.body}
unread (Identifier a) = unreadIdentifier a
unread (IfStatement a) = unreadIfStatement {test: unread a.test, consequent: unread a.consequent, alternate: unreadMaybe a.alternate}
unread (LabeledStatement a) = unreadLabeledStatement {label: unread a.label, body: unread a.body}
unread (LiteralBoolean a) = unreadLiteralBoolean a
unread LiteralNull = unreadLiteralNull
unread (LiteralNumber a) = unreadLiteralNumber a
unread (LiteralRegExp a) = unreadLiteralRegExp a
unread (LiteralString a) = unreadLiteralString a
unread (LogicalExpression a) = unreadLogicalExpression {operator: unreadLogicalOperator a.operator, left: unread a.left, right: unread a.right}
unread (MemberExpression a) = unreadMemberExpression {object: unread a.object, property: unread a.property, computed: a.computed}
unread (NewExpression a) = unreadNewExpression {callee: unread a.callee, arguments: map unread a.arguments}
unread (ObjectExpression a) = unreadObjectExpression {properties: map unreadObjectProperty a.properties}
unread (Program a) = unreadProgram {body: map unread a.body}
unread (ReturnStatement a) = unreadReturnStatement {argument: unreadMaybe a.argument}
unread (SequenceExpression a) = unreadSequenceExpression {expressions: map unread a.expressions}
unread (SwitchCase a) = unreadSwitchCase {test: unreadMaybe a.test, consequent: map unread a.consequent}
unread (SwitchStatement a) = unreadSwitchStatement {discriminant: unread a.discriminant, cases: map unread a.cases}
unread ThisExpression = unreadThisExpression
unread (ThrowStatement a) = unreadThrowStatement {argument: unread a.argument}
unread (TryStatement a) = unreadTryStatement {block: unread a.block, handler: unreadMaybe a.handler, finalizer: unreadMaybe a.finalizer}
unread (UnaryExpression a) = unreadUnaryExpression {operator: unreadUnaryOperator a.operator, argument: unread a.argument}
unread (UpdateExpression a) = unreadUpdateExpression {operator: unreadUpdateOperator a.operator, argument: unread a.argument, prefix: a.prefix}
unread (VariableDeclaration a) = unreadVariableDeclaration {kind: unreadVarDeclKind a.kind, declarations: map unread a.declarations}
unread (VariableDeclarator a) = unreadVariableDeclarator {id: unread a.id, init: unreadMaybe a.init}
unread (WhileStatement a) = unreadWhileStatement {test: unread a.test, body: unread a.body}
unread (WithStatement a) = unreadWithStatement {object: unread a.object, body: unread a.body}


foreign import toJSON "function toJSON(a){ return JSON.stringify(a, null, 2); };" :: forall a. a -> String
instance showNode :: Show Node where
  show a = toJSON $ unread a

-- this will be in Data.String.Regex: https://github.com/purescript/purescript-strings/issues/3
foreign import showRegexP "function showRegexP(r){ return '' + r; }" :: Regex -> String
instance tmpShowRegex :: Show Regex where
  show = showRegexP

instance showVarDeclKind :: Show VarDeclKind where
  show x = show $ unreadVarDeclKind x

instance showObjectPropertyKind :: Show ObjectPropertyKind where
  show x = show $ unreadObjectPropertyKind x

instance showObjectProperty :: Show ObjectProperty where
  show (ObjectProperty a) = "<<Property kind:" ++ show a.kind ++ " key:" ++ show a.key ++ " value:" ++ show a.value ++ ">>"

instance showAssignmentOperator :: Show AssignmentOperator where
  show x = show $ unreadAssignmentOperator x
instance showBinaryOperator :: Show BinaryOperator where
  show x = show $ unreadBinaryOperator x
instance showLogicalOperator :: Show LogicalOperator where
  show x = show $ unreadLogicalOperator x
instance showUnaryOperator :: Show UnaryOperator where
  show x = show $ unreadUnaryOperator x
instance showUpdateOperator :: Show UpdateOperator where
  show x = show $ unreadUpdateOperator x
