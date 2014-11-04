module SpiderMonkeyAST (
  read,
  unread,

  SMAST(),
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


import Data.Array (head)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (Regex())


data VarDeclKind = Var | Let | Const
data ObjectPropertyKind = Init | Get | Set
data ObjectProperty = ObjectProperty { kind :: ObjectPropertyKind, key :: Node, value :: Node }

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
  = ArrayExpression { elements :: [Maybe Node] }
  | AssignmentExpression { operator :: AssignmentOperator, left :: Node, right :: Node }
  | BinaryExpression { operator :: BinaryOperator, left :: Node, right :: Node }
  | BlockStatement { body :: [Node] }
  | BreakStatement { label :: Maybe Node }
  | CallExpression { callee :: Node, arguments :: [Node] }
  | CatchClause { param :: Node, body :: Node }
  | ConditionalExpression { test :: Node, alternate :: Node, consequent :: Node }
  | ContinueStatement { label :: Maybe Node }
  | DebuggerStatement
  | DoWhileStatement { body :: Node, test :: Node }
  | EmptyStatement
  | ExpressionStatement { expression :: Node }
  | ForInStatement { left :: Node, right :: Node, body :: Node }
  | ForStatement { init :: Maybe Node, test :: Maybe Node, update :: Maybe Node, body :: Node }
  | FunctionDeclaration { id :: Node, params :: [Node], body :: Node }
  | FunctionExpression { id :: Maybe Node, params :: [Node], body :: Node }
  | Identifier { name :: String }
  | IfStatement { test :: Node, consequent :: Node, alternate :: Maybe Node }
  | LabeledStatement { label :: Node, body :: Node }
  | LiteralBoolean { value :: Boolean }
  | LiteralNull
  | LiteralNumber { value :: Number }
  | LiteralRegExp { value :: Regex }
  | LiteralString { value :: String }
  | LogicalExpression { operator :: LogicalOperator, left :: Node, right :: Node }
  | MemberExpression { object :: Node, property :: Node, computed :: Boolean }
  | NewExpression { callee :: Node, arguments :: [Node] }
  | ObjectExpression { properties :: [ObjectProperty] }
  | Program { body :: [Node] }
  | ReturnStatement { argument :: Maybe Node }
  | SequenceExpression { expressions :: [Node] }
  | SwitchCase { test :: Maybe Node, consequent :: [Node] }
  | SwitchStatement { discriminant :: Node, cases :: [Node] }
  | ThisExpression
  | ThrowStatement { argument :: Node }
  | TryStatement { block :: Node, handler :: Maybe Node, finalizer :: Maybe Node }
  | UnaryExpression { operator :: UnaryOperator, argument :: Node }
  | UpdateExpression { operator :: UpdateOperator, argument :: Node, prefix :: Boolean }
  | VariableDeclaration { kind :: VarDeclKind, declarations :: [Node] }
  | VariableDeclarator { id :: Node, init :: Maybe Node }
  | WhileStatement { test :: Node, body :: Node }
  | WithStatement { object :: Node, body :: Node }


foreign import data SMAST :: *


readVarDeclKind :: String -> VarDeclKind
readVarDeclKind "var" = Var
readVarDeclKind "let" = Let
readVarDeclKind "const" = Const

readObjectPropertyKind :: String -> ObjectPropertyKind
readObjectPropertyKind "init" = Init
readObjectPropertyKind "get" = Get
readObjectPropertyKind "set" = Set

foreign import readObjectPropertyP "function readObjectPropertyP(node) { return node; }" :: SMAST -> { kind :: String, key :: SMAST, value :: SMAST }
readObjectProperty :: SMAST -> ObjectProperty
readObjectProperty x = ObjectProperty { kind: readObjectPropertyKind(node.kind), key: read node.key, value: read node.value }
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

foreign import throwTypeError "/* istanbul ignore next */ function throwTypeError(msg) { throw new TypeError(msg); }" :: forall a. String ->  a

foreign import fromNullP
  "function fromNullP(Nothing) {\n\
  \  return function (Just) {\n\
  \  return function (x) {\n\
  \    return x == null ? Nothing : Just(x);\n\
  \};};}" :: forall a. Maybe a -> (a -> Maybe a) -> a -> Maybe a
fromNull = fromNullP Nothing Just

-- NOTE: TryStatement interface changed from {handlers: [CatchClause]} to {handler: CatchClause}; we support both
foreign import getHandlers "function getHandlers(node) { return node.handler ? [node.handler] : node.handlers || []; }" :: SMAST -> [SMAST]

foreign import getClass "function getClass(x) { return {}.toString.call(x); }" :: forall a. a -> String
foreign import get "function get(p) { return function(o) { return o[p]; }; }" :: forall a. String -> SMAST -> a
foreign import toBool "function toBool(x) { return !!x; }" :: forall a. a -> Boolean
getType = get "type"

read :: SMAST -> Node
read node = case getType node of

  "ArrayExpression" -> ArrayExpression {
      elements: (\x -> read <$> x) <$> (fromNull <$> get "elements" node)
    }

  "AssignmentExpression" -> AssignmentExpression {
      operator: readAssignmentOperator $ get "operator" node,
      left: read $ get "left" node,
      right: read $ get "right" node
    }

  "BinaryExpression" -> BinaryExpression {
      operator: readBinaryOperator $ get "operator" node,
      left: read $ get "left" node,
      right: read $ get "right" node
    }

  "BlockStatement" -> BlockStatement {
      body: read <$> get "body" node
    }

  "BreakStatement" -> BreakStatement {
      label: read <$> fromNull (get "label" node)
    }

  "CallExpression" -> CallExpression {
      callee: read $ get "callee" node,
      arguments: read <$> get "arguments" node
    }

  "CatchClause" -> CatchClause {
      param: read $ get "param" node,
      body: read $ get "body" node
    }

  "ConditionalExpression" -> ConditionalExpression {
      test: read $ get "test" node,
      alternate: read $ get "alternate" node,
      consequent: read $ get "consequent" node
    }

  "ContinueStatement" -> ContinueStatement {
      label: read <$> fromNull (get "label" node)
    }

  "DebuggerStatement" -> DebuggerStatement

  "DoWhileStatement" -> DoWhileStatement {
      body: read $ get "body" node,
      test: read $ get "test" node
    }

  "EmptyStatement" -> EmptyStatement

  "ExpressionStatement" -> ExpressionStatement {
      expression: read $ get "expression" node
    }

  "ForInStatement" -> ForInStatement {
      left: read $ get "left" node,
      right: read $ get "right" node,
      body: read $ get "body" node
    }

  "ForStatement" -> ForStatement {
      init: read <$> fromNull (get "init" node),
      test: read <$> fromNull (get "test" node),
      update: read <$> fromNull (get "update" node),
      body: read $ get "body" node
    }

  "FunctionDeclaration" -> FunctionDeclaration {
      id: read $ get "id" node,
      params: read <$> get "params" node,
      body: read $ get "body" node
    }

  "FunctionExpression" -> FunctionExpression {
      id: read <$> fromNull (get "id" node),
      params: read <$> get "params" node,
      body: read $ get "body" node
    }

  "Identifier" -> Identifier {
      name: get "name" node
    }

  "IfStatement" -> IfStatement {
      test: read $ get "test" node,
      consequent: read $ get "consequent" node,
      alternate: read <$> fromNull (get "alternate" node)
    }

  "LabeledStatement" -> LabeledStatement {
      label: read $ get "label" node,
      body: read $ get "body" node
    }

  "Literal" -> case getClass $ get "value" node of
    "[object Boolean]" -> LiteralBoolean { value: toBool $ get "value" node }
    "[object Null]" -> LiteralNull
    "[object Number]" -> LiteralNumber { value: get "value" node }
    "[object RegExp]" -> LiteralRegExp { value: get "value" node }
    "[object String]" -> LiteralString { value: get "value" node }

  "LogicalExpression" -> LogicalExpression {
      operator: readLogicalOperator $ get "operator" node,
      left: read $ get "left" node,
      right: read $ get "right" node
    }

  "MemberExpression" -> MemberExpression {
      object: read $ get "object" node,
      property: read $ get "property" node,
      computed: toBool $ get "computed" node
    }

  "NewExpression" -> NewExpression {
      callee: read $ get "callee" node,
      arguments: read <$> get "arguments" node
    }

  "ObjectExpression" -> ObjectExpression {
      properties: readObjectProperty <$> get "properties" node
    }

  "Program" -> Program {
      body: read <$> get "body" node
    }

  "ReturnStatement" -> ReturnStatement {
      argument: read <$> fromNull (get "argument" node)
    }

  "SequenceExpression" -> SequenceExpression {
      expressions: read <$> get "expressions" node
    }

  "SwitchCase" -> SwitchCase {
      test: read <$> fromNull (get "test" node),
      consequent: read <$> get "consequent" node
    }

  "SwitchStatement" -> SwitchStatement {
      discriminant: read $ get "discriminant" node,
      cases: read <$> get "cases" node
    }

  "ThisExpression" -> ThisExpression

  "ThrowStatement" -> ThrowStatement {
      argument: read $ get "argument" node
    }

  "TryStatement" -> TryStatement {
      block: read $ get "block" node,
      handler: read <$> head (getHandlers node),
      finalizer: read <$> fromNull (get "finalizer" node)
    }

  "UnaryExpression" -> UnaryExpression {
      operator: readUnaryOperator $ get "operator" node,
      argument: read $ get "argument" node
    }

  "UpdateExpression" -> UpdateExpression {
      operator: readUpdateOperator $ get "operator" node,
      argument: read $ get "argument" node,
      prefix: toBool $ get "prefix" node
    }

  "VariableDeclaration" -> VariableDeclaration {
      kind: readVarDeclKind $ get "kind" node,
      declarations: read <$> get "declarations" node
    }

  "VariableDeclarator" -> VariableDeclarator {
      id: read $ get "id" node,
      init: read <$> fromNull (get "init" node)
    }

  "WhileStatement" -> WhileStatement {
      test: read $ get "test" node,
      body: read $ get "body" node
    }

  "WithStatement" -> WithStatement {
      object: read $ get "object" node,
      body: read $ get "body" node
    }

  _ -> throwTypeError $ "Unrecognised node type: " <> getType node


foreign import unreadNull "var unreadNull = null;" :: SMAST
unreadMaybe :: Maybe Node -> SMAST
unreadMaybe x = maybe unreadNull unread x

unreadVarDeclKind Var = "var"
unreadVarDeclKind Let = "let"
unreadVarDeclKind Const = "const"

unreadObjectPropertyKind Init = "init"
unreadObjectPropertyKind Get = "get"
unreadObjectPropertyKind Set = "set"

foreign import unreadObjectPropertyP "function unreadObjectPropertyP(node) { return node; }" :: { kind :: String, key :: SMAST, value :: SMAST } -> SMAST
unreadObjectProperty :: ObjectProperty -> SMAST
unreadObjectProperty (ObjectProperty p) = unreadObjectPropertyP { kind: unreadObjectPropertyKind p.kind, key: unread p.key, value: unread p.value }

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

unread (ArrayExpression a) = unreadP {
    "type": "ArrayExpression",
    elements: unreadMaybe <$> a.elements
  }

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

unread (BlockStatement a) = unreadP {
    "type": "BlockStatement",
    body: unread <$> a.body
  }

unread (BreakStatement a) = unreadP {
    "type": "BreakStatement",
    label: unreadMaybe a.label
  }

unread (CallExpression a) = unreadP {
    "type": "CallExpression",
    callee: unread a.callee,
    arguments: unread <$> a.arguments
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

unread (ContinueStatement a) = unreadP {
    "type": "ContinueStatement",
    label: unreadMaybe a.label
  }

unread DebuggerStatement = unreadP {
    "type": "DebuggerStatement"
  }

unread (DoWhileStatement a) = unreadP {
    "type": "DoWhileStatement",
    body: unread a.body,
    test: unread a.test
  }

unread EmptyStatement = unreadP {
    "type": "EmptyStatement"
  }

unread (ExpressionStatement a) = unreadP {
    "type": "ExpressionStatement",
    expression: unread a.expression
  }

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
    params: unread <$> a.params,
    body: unread a.body
  }

unread (FunctionExpression a) = unreadP {
    "type": "FunctionExpression",
    id: unreadMaybe a.id,
    params: unread <$> a.params,
    body: unread a.body
  }

unread (Identifier a) = unreadP {
    "type": "Identifier", name: a.name
  }

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

unread (LiteralBoolean a) = unreadP { "type": "Literal", value: a.value }
unread LiteralNull = unreadP { "type": "Literal", value: unreadNull }
unread (LiteralNumber a) = unreadP { "type": "Literal", value: a.value }
unread (LiteralRegExp a) = unreadP { "type": "Literal", value: a.value }
unread (LiteralString a) = unreadP { "type": "Literal", value: a.value }

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
    arguments: unread <$> a.arguments
  }

unread (ObjectExpression a) = unreadP {
    "type": "ObjectExpression",
    properties: unreadObjectProperty <$> a.properties
  }

unread (Program a) = unreadP {
    "type": "Program",
    body: unread <$> a.body
  }

unread (ReturnStatement a) = unreadP {
    "type": "ReturnStatement",
    argument: unreadMaybe a.argument
  }

unread (SequenceExpression a) = unreadP {
    "type": "SequenceExpression",
    expressions: unread <$> a.expressions
  }

unread (SwitchCase a) = unreadP {
    "type": "SwitchCase",
    test: unreadMaybe a.test,
    consequent: unread <$> a.consequent
  }

unread (SwitchStatement a) = unreadP {
    "type": "SwitchStatement",
    discriminant: unread a.discriminant,
    cases: unread <$> a.cases
  }

unread ThisExpression = unreadP {
    "type": "ThisExpression"
  }

unread (ThrowStatement a) = unreadP {
    "type": "ThrowStatement",
    argument: unread a.argument
  }

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
    declarations: unread <$> a.declarations
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


foreign import toJSON "function toJSON(a) { return JSON.stringify(a, null, 2); }" :: forall a. a -> String
instance showNode :: Show Node where
  show a = toJSON $ unread a
