module HighLevelTests (main) where

import Node.FS.Async (Callback(), readFile)
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

main = suite "high-level tests" $ do

  test "self round-tripping" \done -> do
    readFile (resolve "..") $ testRoundTripAndShow
    return done

  test "everything.js" \done -> do
    readFile (resolve "everything.js") $ testRoundTripAndShow
    return done
