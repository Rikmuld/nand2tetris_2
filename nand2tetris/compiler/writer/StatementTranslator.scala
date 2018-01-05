package nand2tetris.compiler.writer

import nand2tetris.compiler.grammar._
import nand2tetris.compiler.writer.VariableAnalyzer.VarTable

import scala.util.Random
import TranslationHelper._
import nand2tetris.compiler.grammar.LexicalElements.Identifier
import nand2tetris.compiler.writer.ClassTranslator.ArgMap

object StatementTranslator {
  def translate(statement: Statement, current: Identifier, cv: VarTable, rv: VarTable, argCount: ArgMap): Seq[String] = {
    implicit def statementTr(statement: Statement): Seq[String] =
      StatementTranslator.translate(statement, current, cv, rv, argCount)

    implicit def expressionTr(expression: Expression): Seq[String] =
      ExpressionTranslator.translate(expression, current, cv, rv, argCount)

    statement match {
      case DoStatement(call) =>
        ExpressionTranslator.translateRoutineCall(call, current, cv, rv, argCount) ++ Seq("pop temp 0")
      case ReturnStatement(opValue) =>
        optionalElse(constant(0), opValue)(expressionTr) ++ Seq("return")
      case LetStatement(varName, opArray, equals) =>
        if(opArray.isDefined)
          opArray.get ++
          push(varName, cv, rv) ++
          Seq("add") ++
          equals ++
          Seq("pop temp 0", "pop pointer 1", "push temp 0", "pop that 0")
        else equals ++ pop(varName, cv, rv)
      case WhileStatement(condition, whileTrue) =>
        val theLabel = "l" + Random.alphanumeric.take(15).mkString

        label(theLabel + "1") ++
        condition ++
        not ++
        ifGoto(theLabel + "2") ++
        all(whileTrue)(statementTr) ++
        goto(theLabel + "1") ++
        label(theLabel + "2")
      case IfStatement(condition, ifTrue, opElse) =>
        val theLabel = "l" + Random.alphanumeric.take(15).mkString

        condition ++
        not ++
        ifGoto(theLabel + "1") ++
        all(ifTrue)(statementTr) ++
        goto(theLabel + "2") ++
        label(theLabel + "1") ++
        opAll(opElse)(statementTr) ++
        label(theLabel + "2")
    }
  }
}
