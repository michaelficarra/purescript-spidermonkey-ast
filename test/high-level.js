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
      var maybeReadProgram = SpiderMonkeyAST.read(program);
      assert.equal(maybeReadProgram.ctor, 'Data.Maybe.Just');
      assert.equal(maybeReadProgram.values.length, 1);
      assert.equal(maybeReadProgram.values[0].ctor, 'SpiderMonkeyAST.Program');
      var roundTrippedProgram = SpiderMonkeyAST.unread(maybeReadProgram.values[0]);
      assert.equal(escodegen.generate(program), escodegen.generate(roundTrippedProgram));
      done();
    });
  });
});
