package nand2tetris.compiler.grammar

import LexicalElements._

case class Class(name: Identifier, vars: Seq[ClassVarDec], fs: Seq[SubroutineDec]) extends GrammarTree

case class ClassVarDec(kind: Keyword, typ: LexicalElement, varNames: Seq[Identifier]) extends GrammarTree

case class SubroutineDec(kind: Keyword, returnType: LexicalElement, name: Identifier, params: Seq[Parameter], body: SubroutineBody) extends GrammarTree

case class Parameter(typ: LexicalElement, varNames: Identifier) extends GrammarTree

case class SubroutineBody(vars: Seq[VarDec], statements: Seq[Statement]) extends GrammarTree

case class VarDec(typ: LexicalElement, varNames: Seq[Identifier]) extends GrammarTree