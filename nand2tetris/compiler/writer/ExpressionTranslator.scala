package nand2tetris.compiler.writer

import nand2tetris.compiler.grammar._
import nand2tetris.compiler.grammar.LexicalElements._
import nand2tetris.compiler.writer.VariableAnalyzer.VarTable
import TranslationHelper._
import nand2tetris.compiler.writer.ClassTranslator.ArgMap

object ExpressionTranslator {
  def translate(expression: Expression, current: Identifier, classTable: VarTable, routineTable: VarTable, argCount: ArgMap): Seq[String] =
    expression.opTerm.foldLeft(translate(expression.term, current, classTable, routineTable, argCount)){ case(acc, (op, term)) =>
      acc ++ translate(term, current, classTable, routineTable, argCount) ++ operator(op)
    }

  private def translate(term: Term, current: Identifier, cv: VarTable, rv: VarTable, argCount: ArgMap): Seq[String] = term match {
    case ArrayTerm(varName, arrayExp) =>
      ???
    case ExpressionTerm(exp) =>
      translate(exp, current, cv, rv, argCount)
    case SimpleTerm(Identifier(name)) =>
      push(Identifier(name), cv, rv)
    case SimpleTerm(ConstantInteger(int)) =>
      constant(int)
    case SimpleTerm(ConstantString(string)) =>
      ??? //TODO OS function and catch + for string differently
    case SimpleTerm(Keyword(keyword)) => keyword match {
      case THIS =>
        Seq("push pointer 0")
      case FALSE =>
        constant(0)
      case TRUE =>
        constant(-1)
      case NULL =>
        constant(0)
    }
    case SubroutineCallTerm(call) =>
      translateRoutineCall(call, current, cv, rv, argCount)
    case UnitaryOpTerm(op, newTerm) =>
      translate(term, current, cv, rv, argCount) ++ operator(op)
  }

  def translateRoutineCall(call: SubroutineCall, current: Identifier, classTable: VarTable, routineTable: VarTable, argCount: ArgMap): Seq[String] = {
    val params = call.callParameters.flatMap(exp => translate(exp, current, classTable, routineTable, argCount))

    call.prefix match {
      case None =>
        val name = s"${current.str}.${call.routineName}"

        Seq("push pointer 0") ++ params ++
        Seq(s"call $name ${argCount(name) + 1}")
      case Some(variable) if classTable.contains(variable) || routineTable.contains(variable) =>
        val name = s"${(if(classTable.contains(variable)) classTable else routineTable)(variable).typ}.${call.routineName}"

        push(variable, classTable, routineTable) ++ params ++
        Seq(s"call $name ${argCount(name) + 1}")
      case Some(obj) =>
        val name = s"${obj.str}.${call.routineName}"

        params ++ Seq(s"call $name ${argCount(name)}")
    }
  }
}
