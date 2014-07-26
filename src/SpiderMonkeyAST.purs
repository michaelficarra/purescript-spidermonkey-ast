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


foreign import unreadP "function unreadP(x) { return x; }" :: forall a. { "type" :: String | a } -> SMAST

unread :: Node -> SMAST

unread (ArrayExpression a) = unreadP {"type": "ArrayExpression", elements: map unreadMaybe a.elements}

unread (AssignmentExpression a) = unreadP {
    "type": "AssignmentExpression",
    operator: unreadAssignmentOperator a.operator,
    left: unread a.left,
    right: unread a.right
  }

unread (BinaryExpression a) = unreadP {
    "type": "BinaryExpression",
    operator: unreadBinaryOperator a.operator,
    left: unread a.left,
    right: unread a.right
  }

unread (BlockStatement a) = unreadP {"type": "BlockStatement", body: map unread a.body}

unread (BreakStatement a) = unreadP {"type": "BreakStatement", label: unreadMaybe a.label}

unread (CallExpression a) = unreadP {
    "type": "CallExpression",
    callee: unread a.callee,
    arguments: map unread a.arguments
  }

unread (CatchClause a) = unreadP {
    "type": "CatchClause",
    param: unread a.param,
    body: unread a.body
  }

unread (ConditionalExpression a) = unreadP {
    "type": "ConditionalExpression",
    test: unread a.test,
    alternate: unread a.alternate,
    consequent: unread a.consequent
  }

unread (ContinueStatement a) = unreadP {"type": "ContinueStatement", label: unreadMaybe a.label}

unread DebuggerStatement = unreadP {"type": "DebuggerStatement"}

unread (DoWhileStatement a) = unreadP {
    "type": "DoWhileStatement",
    body: unread a.body,
    test: unread a.test
  }

unread EmptyStatement = unreadP {"type": "EmptyStatement"}

unread (ExpressionStatement a) = unreadP {"type": "ExpressionStatement", expression: unread a.expression}

unread (ForInStatement a) = unreadP {
    "type": "ForInStatement",
    left: unread a.left,
    right: unread a.right,
    body: unread a.body
  }

unread (ForStatement a) = unreadP {
    "type": "ForStatement",
    init: unreadMaybe a.init,
    test: unreadMaybe a.test,
    update: unreadMaybe a.update,
    body: unread a.body
  }

unread (FunctionDeclaration a) = unreadP {
    "type": "FunctionDeclaration",
    id: unread a.id,
    params: map unread a.params,
    body: unread a.body
  }

unread (FunctionExpression a) = unreadP {
    "type": "FunctionExpression",
    id: unreadMaybe a.id,
    params: map unread a.params,
    body: unread a.body
  }

unread (Identifier a) = unreadP {"type": "Identifier", name: a.name}

unread (IfStatement a) = unreadP {
    "type": "IfStatement",
    test: unread a.test,
    consequent: unread a.consequent,
    alternate: unreadMaybe a.alternate
  }

unread (LabeledStatement a) = unreadP {
    "type": "LabeledStatement",
    label: unread a.label,
    body: unread a.body
  }

unread (LiteralBoolean a) = unreadP {"type": "Literal", value: a.value}
unread LiteralNull = unreadP {"type": "Literal", value: unreadNull}
unread (LiteralNumber a) = unreadP {"type": "Literal", value: a.value}
unread (LiteralRegExp a) = unreadP {"type": "Literal", value: a.value}
unread (LiteralString a) = unreadP {"type": "Literal", value: a.value}

unread (LogicalExpression a) = unreadP {
    "type": "LogicalExpression",
    operator: unreadLogicalOperator a.operator,
    left: unread a.left,
    right: unread a.right
  }

unread (MemberExpression a) = unreadP {
    "type": "MemberExpression",
    object: unread a.object,
    property: unread a.property,
    computed: a.computed
  }

unread (NewExpression a) = unreadP {
    "type": "NewExpression",
    callee: unread a.callee,
    arguments: map unread a.arguments
  }

unread (ObjectExpression a) = unreadP {"type": "ObjectExpression", properties: map unreadObjectProperty a.properties}

unread (Program a) = unreadP {"type": "Program", body: map unread a.body}

unread (ReturnStatement a) = unreadP {"type": "ReturnStatement", argument: unreadMaybe a.argument}

unread (SequenceExpression a) = unreadP {"type": "SequenceExpression", expressions: map unread a.expressions}

unread (SwitchCase a) = unreadP {
    "type": "SwitchCase",
    test: unreadMaybe a.test,
    consequent: map unread a.consequent
  }

unread (SwitchStatement a) = unreadP {
    "type": "SwitchStatement",
    discriminant: unread a.discriminant,
    cases: map unread a.cases
  }

unread ThisExpression = unreadP {"type": "ThisExpression"}

unread (ThrowStatement a) = unreadP {"type": "ThrowStatement", argument: unread a.argument}

unread (TryStatement a) = unreadP {
    "type": "TryStatement",
    block: unread a.block,
    handler: unreadMaybe a.handler,
    finalizer: unreadMaybe a.finalizer
  }

unread (UnaryExpression a) = unreadP {
    "type": "UnaryExpression",
    operator: unreadUnaryOperator a.operator,
    argument: unread a.argument
  }

unread (UpdateExpression a) = unreadP {
    "type": "UpdateExpression",
    operator: unreadUpdateOperator a.operator,
    argument: unread a.argument,
    prefix: a.prefix
  }

unread (VariableDeclaration a) = unreadP {
    "type": "VariableDeclaration",
    kind: unreadVarDeclKind a.kind,
    declarations: map unread a.declarations
  }

unread (VariableDeclarator a) = unreadP {
    "type": "VariableDeclarator",
    id: unread a.id,
    init: unreadMaybe a.init
  }

unread (WhileStatement a) = unreadP {
    "type": "WhileStatement",
    test: unread a.test,
    body: unread a.body
  }

unread (WithStatement a) = unreadP {
    "type": "WithStatement",
    object: unread a.object,
    body: unread a.body
  }


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
