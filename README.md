# Module Documentation

## Module SpiderMonkeyAST

### Types

    data AssignmentOperator where
      AssignOp :: AssignmentOperator
      AssignOpPlus :: AssignmentOperator
      AssignOpMinus :: AssignmentOperator
      AssignOpMul :: AssignmentOperator
      AssignOpDiv :: AssignmentOperator
      AssignOpMod :: AssignmentOperator
      AssignOpLeftShift :: AssignmentOperator
      AssignOpRightShift :: AssignmentOperator
      AssignOpUnsignedRightShift :: AssignmentOperator
      AssignOpBitOr :: AssignmentOperator
      AssignOpBitXor :: AssignmentOperator
      AssignOpBitAnd :: AssignmentOperator

    data BinaryOperator where
      BinaryOpEQ :: BinaryOperator
      BinaryOpNEQ :: BinaryOperator
      BinaryOpStrictEQ :: BinaryOperator
      BinaryOpStrictNEQ :: BinaryOperator
      BinaryOpLT :: BinaryOperator
      BinaryOpLTE :: BinaryOperator
      BinaryOpGT :: BinaryOperator
      BinaryOpGTE :: BinaryOperator
      BinaryOpLeftShift :: BinaryOperator
      BinaryOpRightShift :: BinaryOperator
      BinaryOpUnsignedRightShift :: BinaryOperator
      BinaryOpPlus :: BinaryOperator
      BinaryOpMinus :: BinaryOperator
      BinaryOpMul :: BinaryOperator
      BinaryOpDiv :: BinaryOperator
      BinaryOpMod :: BinaryOperator
      BinaryOpBitOr :: BinaryOperator
      BinaryOpBitXor :: BinaryOperator
      BinaryOpBitAnd :: BinaryOperator
      BinaryOpIn :: BinaryOperator
      BinaryOpInstanceof :: BinaryOperator

    data LogicalOperator where
      LogicalOpOr :: LogicalOperator
      LogicalOpAnd :: LogicalOperator

    data Node where
      ArrayExpression :: { elements :: [Data.Maybe.Maybe SpiderMonkeyAST.Node] } -> Node
      AssignmentExpression :: { operator :: SpiderMonkeyAST.AssignmentOperator, left :: SpiderMonkeyAST.Node, right :: SpiderMonkeyAST.Node } -> Node
      BinaryExpression :: { operator :: SpiderMonkeyAST.BinaryOperator, left :: SpiderMonkeyAST.Node, right :: SpiderMonkeyAST.Node } -> Node
      BlockStatement :: { body :: [SpiderMonkeyAST.Node] } -> Node
      BreakStatement :: { label :: Data.Maybe.Maybe SpiderMonkeyAST.Node } -> Node
      CallExpression :: { callee :: SpiderMonkeyAST.Node, arguments :: [SpiderMonkeyAST.Node] } -> Node
      CatchClause :: { param :: SpiderMonkeyAST.Node, body :: SpiderMonkeyAST.Node } -> Node
      ConditionalExpression :: { test :: SpiderMonkeyAST.Node, alternate :: SpiderMonkeyAST.Node, consequent :: SpiderMonkeyAST.Node } -> Node
      ContinueStatement :: { label :: Data.Maybe.Maybe SpiderMonkeyAST.Node } -> Node
      DebuggerStatement :: Node
      DoWhileStatement :: { body :: SpiderMonkeyAST.Node, test :: SpiderMonkeyAST.Node } -> Node
      EmptyStatement :: Node
      ExpressionStatement :: { expression :: SpiderMonkeyAST.Node } -> Node
      ForInStatement :: { left :: SpiderMonkeyAST.Node, right :: SpiderMonkeyAST.Node, body :: SpiderMonkeyAST.Node } -> Node
      ForStatement :: { init :: Data.Maybe.Maybe SpiderMonkeyAST.Node, test :: Data.Maybe.Maybe SpiderMonkeyAST.Node, update :: Data.Maybe.Maybe SpiderMonkeyAST.Node, body :: SpiderMonkeyAST.Node } -> Node
      FunctionDeclaration :: { id :: SpiderMonkeyAST.Node, params :: [SpiderMonkeyAST.Node], body :: SpiderMonkeyAST.Node } -> Node
      FunctionExpression :: { id :: Data.Maybe.Maybe SpiderMonkeyAST.Node, params :: [SpiderMonkeyAST.Node], body :: SpiderMonkeyAST.Node } -> Node
      Identifier :: { name :: String } -> Node
      IfStatement :: { test :: SpiderMonkeyAST.Node, consequent :: SpiderMonkeyAST.Node, alternate :: Data.Maybe.Maybe SpiderMonkeyAST.Node } -> Node
      LabeledStatement :: { label :: SpiderMonkeyAST.Node, body :: SpiderMonkeyAST.Node } -> Node
      LiteralBoolean :: { value :: Boolean } -> Node
      LiteralNull :: Node
      LiteralNumber :: { value :: Number } -> Node
      LiteralRegExp :: { value :: Data.String.Regex.Regex } -> Node
      LiteralString :: { value :: String } -> Node
      LogicalExpression :: { operator :: SpiderMonkeyAST.LogicalOperator, left :: SpiderMonkeyAST.Node, right :: SpiderMonkeyAST.Node } -> Node
      MemberExpression :: { object :: SpiderMonkeyAST.Node, property :: SpiderMonkeyAST.Node, computed :: Boolean } -> Node
      NewExpression :: { callee :: SpiderMonkeyAST.Node, arguments :: [SpiderMonkeyAST.Node] } -> Node
      ObjectExpression :: { properties :: [SpiderMonkeyAST.ObjectProperty] } -> Node
      Program :: { body :: [SpiderMonkeyAST.Node] } -> Node
      ReturnStatement :: { argument :: Data.Maybe.Maybe SpiderMonkeyAST.Node } -> Node
      SequenceExpression :: { expressions :: [SpiderMonkeyAST.Node] } -> Node
      SwitchCase :: { test :: Data.Maybe.Maybe SpiderMonkeyAST.Node, consequent :: [SpiderMonkeyAST.Node] } -> Node
      SwitchStatement :: { discriminant :: SpiderMonkeyAST.Node, cases :: [SpiderMonkeyAST.Node] } -> Node
      ThisExpression :: Node
      ThrowStatement :: { argument :: SpiderMonkeyAST.Node } -> Node
      TryStatement :: { block :: SpiderMonkeyAST.Node, handler :: Data.Maybe.Maybe SpiderMonkeyAST.Node, finalizer :: Data.Maybe.Maybe SpiderMonkeyAST.Node } -> Node
      UnaryExpression :: { operator :: SpiderMonkeyAST.UnaryOperator, argument :: SpiderMonkeyAST.Node } -> Node
      UpdateExpression :: { operator :: SpiderMonkeyAST.UpdateOperator, argument :: SpiderMonkeyAST.Node, prefix :: Boolean } -> Node
      VariableDeclaration :: { kind :: SpiderMonkeyAST.VarDeclKind, declarations :: [SpiderMonkeyAST.Node] } -> Node
      VariableDeclarator :: { id :: SpiderMonkeyAST.Node, init :: Data.Maybe.Maybe SpiderMonkeyAST.Node } -> Node
      WhileStatement :: { test :: SpiderMonkeyAST.Node, body :: SpiderMonkeyAST.Node } -> Node
      WithStatement :: { object :: SpiderMonkeyAST.Node, body :: SpiderMonkeyAST.Node } -> Node

    data ObjectProperty where
      ObjectProperty :: { kind :: SpiderMonkeyAST.ObjectPropertyKind, key :: SpiderMonkeyAST.Node, value :: SpiderMonkeyAST.Node } -> ObjectProperty

    data ObjectPropertyKind where
      Init :: ObjectPropertyKind
      Get :: ObjectPropertyKind
      Set :: ObjectPropertyKind

    data SMAST :: *

    data UnaryOperator where
      UnaryOpMinus :: UnaryOperator
      UnaryOpPlus :: UnaryOperator
      UnaryOpLogicalNot :: UnaryOperator
      UnaryOpBitNot :: UnaryOperator
      UnaryOpTypeof :: UnaryOperator
      UnaryOpVoid :: UnaryOperator
      UnaryOpDelete :: UnaryOperator

    data UpdateOperator where
      UpdateOpIncrement :: UpdateOperator
      UpdateOpDecrement :: UpdateOperator

    data VarDeclKind where
      Var :: VarDeclKind
      Let :: VarDeclKind
      Const :: VarDeclKind


### Values

    read :: SpiderMonkeyAST.SMAST -> Data.Maybe.Maybe SpiderMonkeyAST.Node

    readExpr :: SpiderMonkeyAST.SMAST -> Data.Maybe.Maybe SpiderMonkeyAST.Node

    unread :: SpiderMonkeyAST.Node -> SpiderMonkeyAST.SMAST



