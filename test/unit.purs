module HighLevelTests (main) where

import Node.FS.Async (Callback(), readFile)
import Data.Either (Either(..))
import qualified Node.Buffer as Buffer
import qualified Node.Encoding as Encoding

import TestHelper (read, unread, SMAST(), suite, test, eq, resolve, esprima, escodegen, jsonStringify)

foreign import toSMAST "function toSMAST(x) { return x; }" :: forall r. {"type" :: String | r} -> SMAST

main = suite "unit" $ do

  test "TryStatement handler/handlers" \done -> do
    let block = {"type": "BlockStatement", body: []}
    let catchClause = {"type": "CatchClause", param: {"type": "Identifier", name: "e"}, body: block}

    let withHandler = unread $ read $ toSMAST {"type": "TryStatement", block: block, handler: catchClause}
    let withHandlers = unread $ read $ toSMAST {"type": "TryStatement", block: block, handlers: [catchClause]}
    let withTwoHandlers = unread $ read $ toSMAST {"type": "TryStatement", block: block, handlers: [catchClause]}
    let withBoth = unread $ read $ toSMAST {"type": "TryStatement", block: block, handler: catchClause, handlers: [catchClause]}

    let expectedCatch = escodegen.generate $ toSMAST {"type": "TryStatement", block: block, handler: catchClause}
    eq (escodegen.generate withHandler) expectedCatch
    eq (escodegen.generate withHandlers) expectedCatch
    eq (escodegen.generate withTwoHandlers) expectedCatch
    eq (escodegen.generate withBoth) expectedCatch

    let withEmptyHandlers = unread $ read $ toSMAST {"type": "TryStatement", block: block, handlers: [], finalizer: block}
    let withNeither = unread $ read $ toSMAST {"type": "TryStatement", block: block, finalizer: block}

    let expectedFinally = escodegen.generate $ toSMAST {"type": "TryStatement", block: block, finalizer: block}
    eq (escodegen.generate withEmptyHandlers) expectedFinally
    eq (escodegen.generate withNeither) expectedFinally
    return done

  test "VariableDeclaration kinds: var/let/const" \done -> do
    let declarator = {"type": "VariableDeclarator", id: {"type": "Identifier", name: "a"}}
    let withVar = toSMAST {"type": "VariableDeclaration", kind: "var", declarations: [declarator]}
    let withLet = toSMAST {"type": "VariableDeclaration", kind: "let", declarations: [declarator]}
    let withConst = toSMAST {"type": "VariableDeclaration", kind: "const", declarations: [declarator]}
    eq (escodegen.generate withVar) (escodegen.generate $ unread $ read withVar)
    eq (escodegen.generate withLet) (escodegen.generate $ unread $ read withLet)
    eq (escodegen.generate withConst) (escodegen.generate $ unread $ read withConst)
    return done
