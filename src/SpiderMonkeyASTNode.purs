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
type ObjectProperty = {key :: Node, value :: Node, kind :: ObjectPropertyKind}

data UnaryOperator
  = UnaryOpMinus | UnaryOpPlus | UnaryOpLogicalNot | UnaryOpBitNot
  | UnaryOpTypeof | UnaryOpVoid | UnaryOpDelete
data BinaryOperator
  = BinaryOpEQ | BinaryOpNEQ | BinaryOpStrictEQ | BinaryOpStrictNEQ
  | BinaryOpLT | BinaryOpLTE | BinaryOpGT | BinaryOpGTE
  | BinaryOpLeftShift | BinaryOpRightShift | BinaryOpUnsignedRightShift
  | BinaryOpPlus | BinaryOpMinus | BinaryOpMul | BinaryOpDiv | BinaryOpMod
  | BinaryOpBitOr | BinaryOpBitXor | BinaryOpBitAnd
  | BinaryOpIn | BinaryOpInstanceof
data LogicalOperator = LogicalOpOr | LogicalOpAnd
data UpdateOperator = UpdateOpIncrement | UpdateOpDecrement
data AssignmentOperator
  = AssignOp | AssignOpPlus | AssignOpMinus | AssignOpMul | AssignOpDiv | AssignOpMod
  | AssignOpLeftShift | AssignOpRightShift | AssignOpUnsignedRightShift
  | AssignOpBitOr | AssignOpBitXor | AssignOpBitAnd

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


foreign import read
  "function read(node) {\n\
  \  switch(node.type) {\n\
  \  case 'EmptyStatement': return Just(EmptyStatement);\n\
  \  case 'Program': return Just(Program({body: map(read)(node.body)}));\n\
  \  case 'ReturnStatement': return Just(ReturnStatement({argument: node.argument == null ? Nothing : Just(read(node.argument))}));\n\
  \  case 'ThrowStatement': return Just(ThrowStatement({argument: read(node.argument)}));\n\
  \  }\n\
  \  return Nothing;\n\
  \}" :: SMAST -> Maybe Node


foreign import unreadNull "var unreadNull = null;" :: SMAST
unreadMaybe :: Maybe Node -> SMAST
-- why can't this be point-free?
unreadMaybe x = maybe unreadNull unread x

foreign import unreadEmptyStatement
  "var unreadEmptyStatement = {type: 'EmptyStatement'};" :: SMAST

foreign import unreadProgram
  "function unreadProgram(node) {\n\
  \  return {type: 'Program', body: node.body};\n\
  \}" :: {body :: [SMAST]} -> SMAST

foreign import unreadReturnStatement
  "function unreadReturnStatement(node) {\n\
  \  return {type: 'ReturnStatement', argument: node.argument};\n\
  \}" :: {argument :: SMAST} -> SMAST

foreign import unreadThrowStatement
  "function unreadThrowStatement(node) {\n\
  \  return {type: 'ThrowStatement', argument: node.argument};\n\
  \}" :: {argument :: SMAST} -> SMAST

unread :: Node -> SMAST
unread EmptyStatement = unreadEmptyStatement
unread (Program a) = unreadProgram {body: map unread a.body}
unread (ReturnStatement a) = unreadReturnStatement {argument: unreadMaybe a.argument}
unread (ThrowStatement a) = unreadThrowStatement {argument: unread a.argument}


instance showNode :: Show Node where
  show EmptyStatement = "<<EmptyStatement>>"
  show (Program a) = "<<Program body:" ++ show a.body ++ ">>"
  show (ThrowStatement a) = "<<ThrowStatement argument:" ++ show a.argument ++ ">>"
  show (ReturnStatement a) = "<<ReturnStatement argument:" ++ show a.argument ++ ">>"
  show _ = "<<unknown>>"
