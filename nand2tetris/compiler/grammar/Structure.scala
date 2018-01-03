package nand2tetris.compiler.grammar

import LexicalElements._

case class Class(name: Identifier, vars: Seq[ClassVariable], fs: Seq[ClassRoutine]) extends GrammarTree

case class ClassVariable(kind: Keyword, typ: LexicalElement, varNames: Seq[Identifier]) extends GrammarTree

case class ClassRoutine(kind: Keyword, returnType: LexicalElement, name: Identifier, params: Seq[Parameter], body: SubroutineBody) extends GrammarTree

case class Parameter(typ: LexicalElement, varNames: Identifier) extends GrammarTree

case class SubroutineBody(vars: Seq[VarDec], statements: Seq[Statement]) extends GrammarTree

case class VarDec(typ: LexicalElement, varNames: Seq[Identifier]) extends GrammarTree