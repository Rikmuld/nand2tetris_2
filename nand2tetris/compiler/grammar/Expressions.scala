package nand2tetris.compiler.grammar

import LexicalElements._

trait Term extends GrammarTree

case class Expression(term: Term, opTerm: Seq[(Symbol, Term)]) extends GrammarTree

case class ExpressionTerm(exp: Expression) extends Term

case class SubroutineCall(prefix: Option[Identifier], routineName: Identifier, callParameters: Seq[Expression]) extends Term

case class SimpleTerm(lexical: LexicalElement) extends Term

case class ArrayTerm(varName: Identifier, arrayExp: Expression) extends Term

case class UnitaryOpTerm(op: Symbol, term: Term) extends Term
