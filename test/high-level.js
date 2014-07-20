"use strict";

var assert = require("assert");
var fs = require("fs");

var esprima = require("esprima");
var escodegen = require("escodegen");
var SpiderMonkeyAST = require("lib/SpiderMonkeyAST").SpiderMonkeyAST;

suite("high-level tests", function() {
  test("self round-tripping", function(done) {
    fs.readFile(require.resolve(".."), function(err, programText) {
      var program = esprima.parse("" + programText);
      var roundTrippedProgram = SpiderMonkeyAST.unread(SpiderMonkeyAST.read(program));
      assert.equal(escodegen.generate(program), escodegen.generate(roundTrippedProgram));
      done();
    });
  });
});
