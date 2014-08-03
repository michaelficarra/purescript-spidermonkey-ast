module Tests (main) where

import Node.FS.Sync (readFile)
import Data.Either (Either(..))
import qualified Node.Buffer as Buffer
import qualified Node.Encoding as Encoding

import TestHelper (read, unread, suite, test, eq, resolve, esprima, escodegen, jsonStringify)

testRoundTripAndShow (Left err) = return unit
testRoundTripAndShow (Right programText) = do
  let program = esprima.parse (Buffer.toString Encoding.UTF8 programText)
  let roundTripped = unread (read program)
  eq (jsonStringify roundTripped) (show $ read program)
  eq (escodegen.generate program) (escodegen.generate roundTripped)
  return unit

main = suite "high-level tests" do

  test "self round-tripping" do
    readFile (resolve "..") >>= testRoundTripAndShow

  test "everything.js" do
    readFile (resolve "everything.js") >>= testRoundTripAndShow
