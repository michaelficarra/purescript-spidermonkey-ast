module TestHelper where

import Control.Monad.Eff
import Node.Path (FilePath())

foreign import data SMAST :: *
foreign import data Node :: *
foreign import showNode "var showNode = require('..').SpiderMonkeyAST.showNode;" :: Node -> SMAST
foreign import instance showNode :: Show Node
foreign import unread "var unread = require('..').SpiderMonkeyAST.unread;" :: Node -> SMAST
foreign import read "var read = require('..').SpiderMonkeyAST.read;" :: SMAST -> Node

foreign import data Assertion :: !
foreign import data Suite :: !
foreign import data Test :: !
foreign import data Done :: !

foreign import resolve "function resolve(s) { return require.resolve(s); }" :: String -> FilePath
foreign import esprima "var esprima = require('esprima');" :: { parse :: String -> SMAST }
foreign import escodegen "var escodegen = require('escodegen');" :: { generate :: SMAST -> String }
foreign import jsonStringify "function jsonStringify(x) { return JSON.stringify(x, null, 2); }" :: forall a. a -> String

foreign import eq "function eq(a) { return function(b) { return function() { return require('assert').equal(a, b); }; }; }" :: forall a e. a ->  a -> (Eff (assertion :: Assertion | e) Unit)
foreign import suite "function suite(desc) { return function(f) { return describe(desc, f); } }" :: forall e. String -> Eff e Unit -> Eff (suite :: Suite | e) Unit
foreign import test "function test(desc) { return function(f) { return function() { return it(desc, function(done) { f(done)()(); }); }; }; }" :: forall e done. String -> (done -> Eff e done) -> Eff (test :: Test | e) Unit
