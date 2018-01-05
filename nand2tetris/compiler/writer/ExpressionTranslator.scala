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
      translate(arrayExp, current, cv, rv, argCount) ++
      push(varName, cv, rv) ++
      Seq("add", "pop pointer 1", "push that 0")
    case ExpressionTerm(exp) =>
      translate(exp, current, cv, rv, argCount)
    case SimpleTerm(Identifier(name)) =>
      push(Identifier(name), cv, rv)
    case SimpleTerm(ConstantInteger(int)) =>
      constant(int)
    case SimpleTerm(ConstantString(string)) =>
      constant(string.length) ++ Seq("call String.new 1") ++ string.flatMap(char => {
        constant(char.toInt) ++
        Seq("call String.appendChar 2")
      })
    case SimpleTerm(keyword) => keyword match {
      case THIS =>
        Seq("push pointer 0")
      case FALSE =>
        constant(0)
      case TRUE =>
        constant(0) ++ Seq("not")
      case NULL =>
        constant(0)
    }
    case SubroutineCallTerm(call) =>
      translateRoutineCall(call, current, cv, rv, argCount)
    case UnitaryOpTerm(op, newTerm) =>
      translate(newTerm, current, cv, rv, argCount) ++ unitary(op)
  }

  def translateRoutineCall(call: SubroutineCall, current: Identifier, classTable: VarTable, routineTable: VarTable, argCount: ArgMap): Seq[String] = {
    val params = call.callParameters.flatMap(exp => translate(exp, current, classTable, routineTable, argCount))

    call.prefix match {
      case None =>
        val name = s"${current.str}.${call.routineName.str}"

        Seq("push pointer 0") ++ params ++
        Seq(s"call $name ${argCount(name) + 1}")
      case Some(variable) if classTable.contains(variable) || routineTable.contains(variable) =>
        val name = s"${(if(classTable.contains(variable)) classTable else routineTable)(variable).typ.stringValue}.${call.routineName.str}"

        push(variable, classTable, routineTable) ++ params ++
        Seq(s"call $name ${argCount(name) + 1}")
      case Some(obj) =>
        val name = s"${obj.str}.${call.routineName.str}"

        params ++ Seq(s"call $name ${argCount(name)}")
    }
  }
}
