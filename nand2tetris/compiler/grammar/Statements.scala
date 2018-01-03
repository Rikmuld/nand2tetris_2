package nand2tetris.compiler.grammar

import LexicalElements._

trait Statement extends GrammarTree

case class LetStatement(varName: Identifier, optArray: Option[Expression], equals: Expression) extends Statement

case class IfStatement(condition: Expression, ifTrue: Seq[Statement], ifFalse: Option[Seq[Statement]]) extends Statement

case class DoStatement(call: SubroutineCall) extends Statement

case class WhileStatement(condition: Expression, whileTrue: Seq[Statement]) extends Statement

case class ReturnStatement(returnValue: Option[Expression]) extends Statement