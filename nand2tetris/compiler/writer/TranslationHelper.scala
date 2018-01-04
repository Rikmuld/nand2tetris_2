package nand2tetris.compiler.writer

import nand2tetris.compiler.grammar.LexicalElements.Identifier
import nand2tetris.compiler.writer.VariableAnalyzer.VarTable
import nand2tetris.compiler.grammar.LexicalElements._

object TranslationHelper {
  val not: Seq[String] =
    Seq("not")

  def label(label: String): Seq[String] =
    Seq(s"label $label")

  def ifGoto(label: String): Seq[String] =
    Seq(s"if-goto $label")

  def goto(label: String): Seq[String] =
    Seq(s"goto $label")

  def all[A](sa: Seq[A])(f: A => Seq[String]): Seq[String] =
    sa flatMap f

  def opAll[A](sa: Option[Seq[A]])(f: A => Seq[String]): Seq[String] =
    sa.fold(Seq[String]())(_ flatMap f)

  def optional[A](sa: Option[A])(f: A => Seq[String]): Seq[String] =
    optionalElse(Seq(), sa)(f)

  def optionalElse[A](z: Seq[String], sa: Option[A])(f: A => Seq[String]): Seq[String] =
    sa.fold(z)(f(_))

  def pop(name: Identifier, classTable: VarTable, routineTable: VarTable): Seq[String] =
    VariableAnalyzer.translate(name, false, classTable, routineTable)

  def push(name: Identifier, classTable: VarTable, routineTable: VarTable): Seq[String] =
    VariableAnalyzer.translate(name, true, classTable, routineTable)

  def operator(op: Symbol): Seq[String] =
    Seq(op.char.toString) //TODO replace * and / with os functions

  def constant(int: Int): Seq[String] =
    Seq(s"push constant $int")
}
