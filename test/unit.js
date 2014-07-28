"use strict";

var assert = require("assert");
var fs = require("fs");

var esprima = require("esprima");
var escodegen = require("escodegen");
var SpiderMonkeyAST = require("lib/SpiderMonkeyAST").SpiderMonkeyAST;

suite("unit", function() {
  test("TryStatement handler/handlers", function() {
    var block = {type: "BlockStatement", body: []};
    var catchClause = {type: "CatchClause", param: {type: "Identifier", name: "e"}, body: block};

    var withHandler = SpiderMonkeyAST.unread(SpiderMonkeyAST.read({type: "TryStatement", block: block, handler: catchClause}))
    var withHandlers = SpiderMonkeyAST.unread(SpiderMonkeyAST.read({type: "TryStatement", block: block, handlers: [catchClause]}))
    var withTwoHandlers = SpiderMonkeyAST.unread(SpiderMonkeyAST.read({type: "TryStatement", block: block, handlers: [catchClause]}))
    var withBoth = SpiderMonkeyAST.unread(SpiderMonkeyAST.read({type: "TryStatement", block: block, handler: catchClause, handlers: [catchClause]}))

    var expected = escodegen.generate({type: "TryStatement", block: block, handler: catchClause});
    assert.equal(escodegen.generate(withHandler), expected);
    assert.equal(escodegen.generate(withHandlers), expected);
    assert.equal(escodegen.generate(withTwoHandlers), expected);

    var withEmptyHandlers = SpiderMonkeyAST.unread(SpiderMonkeyAST.read({type: "TryStatement", block: block, handlers: [], finalizer: block}))
    var withNeither = SpiderMonkeyAST.unread(SpiderMonkeyAST.read({type: "TryStatement", block: block, finalizer: block}))

    expected = escodegen.generate({type: "TryStatement", block: block, finalizer: block});
    assert.equal(escodegen.generate(withEmptyHandlers), expected);
    assert.equal(escodegen.generate(withNeither), expected);
  });

  test("VariableDeclaration kinds: var/let/const", function() {
    var declarator = {type: "VariableDeclarator", id: {type: "Identifier", name: "a"}};
    var withVar = {type: "VariableDeclaration", kind: "var", declarations: [declarator]};
    var withLet = {type: "VariableDeclaration", kind: "let", declarations: [declarator]};
    var withConst = {type: "VariableDeclaration", kind: "const", declarations: [declarator]};
    assert.equal(escodegen.generate(withVar), escodegen.generate(SpiderMonkeyAST.unread(SpiderMonkeyAST.read(withVar))));
    assert.equal(escodegen.generate(withLet), escodegen.generate(SpiderMonkeyAST.unread(SpiderMonkeyAST.read(withLet))));
    assert.equal(escodegen.generate(withConst), escodegen.generate(SpiderMonkeyAST.unread(SpiderMonkeyAST.read(withConst))));
  });
})
