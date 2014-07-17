module SpiderMonkeyAST (
  read,
  readExpr,
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
type ObjectProperty = {key :: Node, value :: Node, kind :: ObjectPropertyKind}

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
  | VariableDeclaration {declarations :: [Node], kind :: VarDeclKind}
  | VariableDeclarator {id :: Node, init :: Maybe Node}
  | WhileStatement {test :: Node, body :: Node}
  | WithStatement {object :: Node, body :: Node}


foreign import data SMAST :: *


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
  "function readP(node) {\n\
  \  switch(node.type) {\n\
  \  case 'ArrayExpression': return ArrayExpression({elements: [].map.call(node.elements, function(e){ return e == null ? Nothing : Just(readP(e)); })});\n\
  \  case 'AssignmentExpression': return AssignmentExpression({operator: readAssignmentOperator(node.operator), left: readP(node.left), right: readP(node.right)});\n\
  \  case 'BinaryExpression': return BinaryExpression({operator: readBinaryOperator(node.operator), left: readP(node.left), right: readP(node.right)});\n\
  \  case 'BlockStatement': return BlockStatement({body: [].map.call(node.body, readP);});\n\
  \  case 'BreakStatement': return BreakStatement({label: node.label == null ? Nothing : Just(readP(node.label))});\n\
  \  case 'CallExpression': return CallExpression({callee: readP(node.callee), arguments: [].map.call(node.arguments)});\n\
  \  case 'CatchClause': return CatchClause({param: readP(node.param), body: readP(node.body)});\n\
  \  case 'ConditionalExpression': return ConditionalExpression({test: readP(node.test), alternate: readP(node.alternate), consequent: readP(node.consequent)});\n\
  \  case 'ContinueStatement': return ContinueStatement({label: node.label == null ? Nothing : Just(readP(node.label));});\n\
  \  case 'DebuggerStatement': return DebuggerStatement;\n\
  \  case 'DoWhileStatement': return DoWhileStatement({body: readP(node.body), test: readP(node.test)});\n\
  \  case 'EmptyStatement': return EmptyStatement;\n\
  \  case 'ExpressionStatement': return ExpressionStatement({expression: readP(node.expression)});\n\
  \  case 'ForInStatement': return ForInStatement({left: readP(node.left), right: readP(node.right), body: readP(node.body)});\n\
  \  case 'ForStatement': return ForStatement({init: node.init == null ? Nothing : Just(readP(node.init)), test: node.test == null ? Nothing : Just(readP(node.test)), update: node.update == null ? Nothing : Just(readP(node.update)), body: readP(node.body)});\n\
  \  case 'FunctionDeclaration': return FunctionDeclaration({id: readP(node.id), params: [].map.call(node.params, readP), body: readP(node.body)});\n\
  \  case 'FunctionExpression': return FunctionExpression({id: node.id == null ? Nothing : Just(readP(node.id)), params: [].map.call(node.params, readP), body: readP(node.body)});\n\
  \  case 'Literal':\n\
  \    switch({}.toString.call(node.value)) {\n\
  \      case '[object Boolean]': return LiteralBoolean({value: node.value});\n\
  \      case '[object Null]': return LiteralNull;\n\
  \      case '[object Number]': return LiteralNumber({value: node.value});\n\
  \      case '[object RegExp]': return LiteralRegExp({value: node.value});\n\
  \      case '[object String]': return LiteralString({value: node.value});\n\
  \    }\n\
  \  case 'LogicalExpression': return LogicalExpression({operator: readLogicalOperator(node.operator), left: readP(node.left), right: readP(node.right)});\n\
  \  case 'MemberExpression': return MemberExpression({object: readP(node.object), property: readP(node.property), computed: !!node.computed});\n\
  \  case 'NewExpression': return NewExpression({callee: readP(node.callee), arguments: [].map.call(node.arguments)});\n\
  \  case 'Program': return Program({body: [].map.call(node.body, readP);});\n\
  \  case 'ReturnStatement': return ReturnStatement({argument: node.argument == null ? Nothing : Just(readP(node.argument))});\n\
  \  case 'ThisExpression': return ThisExpression;\n\
  \  case 'ThrowStatement': return ThrowStatement({argument: readP(node.argument)});\n\
  \  case 'UnaryExpression': return UnaryExpression({operator: readUnaryOperator(node.operator), argument: readP(node.argument)});\n\
  \  case 'UpdateExpression': return UpdateExpression({operator: readUpdateOperator(node.operator), argument: readP(node.argument), prefix: node.prefix});\n\
  \  case 'WhileStatement': return WhileStatement({test: readP(node.test), body: readP(node.body)});\n\
  \  }\n\
  \  throw new TypeError('Unrecognised node type: ' + JSON.stringify(node.type));\n\
  \}" :: SMAST -> Node

foreign import isValid "var isValid = require('esvalid').isValid" :: SMAST -> Boolean
foreign import isValidExpr "var isValidExpr = require('esvalid').isValidExpr" :: SMAST -> Boolean

read :: SMAST -> Maybe Node
read a | isValid a = Just $ readP a
read _ = Nothing

readExpr :: SMAST -> Maybe Node
readExpr a | isValidExpr a = Just $ readP a
readExpr _ = Nothing


foreign import unreadNull "var unreadNull = null;" :: SMAST
unreadMaybe :: Maybe Node -> SMAST
-- why can't this be point-free?
unreadMaybe x = maybe unreadNull unread x


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
  \  return {type: 'ConditionalExpression', test: node.test, alternate: node.alternate, consequent, node.consequent};\n\
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

foreign import unreadProgram
  "function unreadProgram(node) {\n\
  \  return {type: 'Program', body: node.body};\n\
  \}" :: {body :: [SMAST]} -> SMAST

foreign import unreadReturnStatement
  "function unreadReturnStatement(node) {\n\
  \  return {type: 'ReturnStatement', argument: node.argument};\n\
  \}" :: {argument :: SMAST} -> SMAST

foreign import unreadThisExpression
  "var unreadThisExpression = {type: 'ThisExpression'};" :: SMAST

foreign import unreadThrowStatement
  "function unreadThrowStatement(node) {\n\
  \  return {type: 'ThrowStatement', argument: node.argument};\n\
  \}" :: {argument :: SMAST} -> SMAST

foreign import unreadUnaryExpression
  "function unreadUnaryExpression(node) {\n\
  \  return {type: 'UnaryExpression', operator: node.operator, argument: node.argument};\n\
  \}" :: {operator :: String, argument :: SMAST} -> SMAST

foreign import unreadUpdateExpression
  "function unreadUpdateExpression(node) {\n\
  \  return {type: 'UpdateExpression', operator: node.operator, argument: node.argument, prefix: node.prefix};\n\
  \}" :: {operator :: String, argument :: SMAST, prefix :: Boolean} -> SMAST

foreign import unreadWhileStatement
  "function unreadWhileStatement(node) {\n\
  \  return {type: 'WhileStatement', test: node.test, body: node.body};\n\
  \}" :: {test :: SMAST, body :: SMAST} -> SMAST

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
unread (LiteralBoolean a) = unreadLiteralBoolean a
unread LiteralNull = unreadLiteralNull
unread (LiteralNumber a) = unreadLiteralNumber a
unread (LiteralRegExp a) = unreadLiteralRegExp a
unread (LiteralString a) = unreadLiteralString a
unread (LogicalExpression a) = unreadLogicalExpression {operator: unreadLogicalOperator a.operator, left: unread a.left, right: unread a.right}
unread (MemberExpression a) = unreadMemberExpression {object: unread a.object, property: unread a.property, computed: a.computed}
unread (NewExpression a) = unreadNewExpression {callee: unread a.callee, arguments: map unread a.arguments}
unread (Program a) = unreadProgram {body: map unread a.body}
unread (ReturnStatement a) = unreadReturnStatement {argument: unreadMaybe a.argument}
unread ThisExpression = unreadThisExpression
unread (ThrowStatement a) = unreadThrowStatement {argument: unread a.argument}
unread (UnaryExpression a) = unreadUnaryExpression {operator: unreadUnaryOperator a.operator, argument: unread a.argument}
unread (UpdateExpression a) = unreadUpdateExpression {operator: unreadUpdateOperator a.operator, argument: unread a.argument, prefix: a.prefix}
unread (WhileStatement a) = unreadWhileStatement {test: unread a.test, body: unread a.body}


instance showNode :: Show Node where
  show (ArrayExpression a) = "<<ArrayExpression elements:" ++ show a.elements ++ ">>"
  show (AssignmentExpression a) = "<<AssignmentExpression operator:" ++ show a.operator ++ " left:" ++ show a.left ++ " right:" ++ show a.right ++ ">>"
  show (BinaryExpression a) = "<<BinaryExpression operator:" ++ show a.operator ++ " left:" ++ show a.left ++ " right:" ++ show a.right ++ ">>"
  show (BlockStatement a) = "<<BlockStatement body:" ++ show a.body ++ ">>"
  show (BreakStatement a) = "<<BreakStatement label:" ++ show a.label ++ ">>"
  show (CallExpression a) = "<<CallExpression callee:" ++ show a.callee ++ " arguments:" ++ show a.arguments ++ ">>"
  show (CatchClause a) = "<<CatchClause param:" ++ show a.param ++ " body:" ++ show a.body ++ ">>"
  show (ConditionalExpression a) = "<<ConditionalExpression test:" ++ show a.test ++ " alternate:" ++ show a.alternate ++ " consequent:" ++ show a.consequent ++ ">>"
  show (ContinueStatement a) = "<<ContinueStatement label:" ++ show a.label ++ ">>"
  show DebuggerStatement = "<<DebuggerStatement>>"
  show (DoWhileStatement a) = "<<DoWhileStatement body:" ++ show a.body ++ " test:" ++ show a.test ++ ">>"
  show EmptyStatement = "<<EmptyStatement>>"
  show (ExpressionStatement a) = "<<ExpressionStatement expression:" ++ show a.expression ++ ">>"
  show (ForInStatement a) = "<<ForInStatement left:" ++ show a.left ++ " right:" ++ show a.right ++ " body:" ++ show a.body ++ ">>"
  show (ForStatement a) = "<<ForStatement init:" ++ show a.init ++ " test:" ++ show a.test ++ " update:" ++ show a.update ++ " body:" ++ show a.body ++ ">>"
  show (FunctionDeclaration a) = "<<FunctionDeclaration id:" ++ show a.id ++ " params:" ++ show a.params ++ " body:" ++ show a.body ++ ">>"
  show (FunctionExpression a) = "<<FunctionExpression id:" ++ show a.id ++ " params:" ++ show a.params ++ " body:" ++ show a.body ++ ">>"
  show (LiteralBoolean a) = show a.value
  show LiteralNull = "null"
  show (LiteralNumber a) = show a.value
  show (LiteralRegExp a) = show a.value
  show (LiteralString a) = show a.value
  show (LogicalExpression a) = "<<LogicalExpression operator:" ++ show a.operator ++ " left:" ++ show a.left ++ " right:" ++ show a.right ++ ">>"
  show (MemberExpression a) = "<<MemberExpression object:" ++ show a.object ++ " property:" ++ show a.property ++ " computed:" ++ show a.computed ++ ">>"
  show (NewExpression a) = "<<NewExpression callee:" ++ show a.callee ++ " arguments:" ++ show a.arguments ++ ">>"
  show (Program a) = "<<Program body:" ++ show a.body ++ ">>"
  show (ReturnStatement a) = "<<ReturnStatement argument:" ++ show a.argument ++ ">>"
  show ThisExpression = "<<ThisExpression>>"
  show (ThrowStatement a) = "<<ThrowStatement argument:" ++ show a.argument ++ ">>"
  show (UnaryExpression a) = "<<UnaryExpression operator:" ++ show a.operator ++ " argument:" ++ show a.argument ++ ">>"
  show (UpdateExpression a) = "<<UpdateExpression operator:" ++ show a.operator ++ " argument:" ++ show a.argument ++ " prefix:" ++ show a.prefix ++ ">>"
  show (WhileStatement a) = "<<WhileStatement test:" ++ show a.test ++ " body:" ++ show a.body ++ ">>"
  show _ = "<<unknown>>"

-- this will be in Data.String.Regex: https://github.com/purescript/purescript-strings/issues/3
foreign import showRegexP
  "function showRegexP(r){\n\
  \  return r.toString();\n\
  \}" :: Regex -> String
instance tmpShowRegex :: Show Regex where
  show = showRegexP

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
