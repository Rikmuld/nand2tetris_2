package nand2tetris.compiler.writer

import nand2tetris.compiler.grammar.LexicalElements._
import nand2tetris.compiler.grammar.{ClassVarDec, SubroutineDec}

object VariableAnalyzer {
  type VarTable = Map[Identifier, VariableInfo]

  def createClassTable(vars: Seq[ClassVarDec]): Map[Identifier, VariableInfo] = {
    val scopeCounter = scala.collection.mutable.Map(STATIC -> 0, FIELD -> 0)

    vars.flatMap(varDec => {
      varDec.varNames.map(name => {
        val index = scopeCounter(varDec.kind)
        scopeCounter.update(varDec.kind, index + 1)

        name -> VariableInfo(varDec.typ, if (varDec.kind == STATIC) Static else Field, index)
      })
    }).toMap
  }

  def createRoutineTable(routine: SubroutineDec, current: Identifier, kind: Keyword): Map[Identifier, VariableInfo] = {
    var counter = 0

    routine.params.zipWithIndex.map {
      case (param, index) =>
        param.varName -> VariableInfo(param.typ, Argument, index + (if (kind == METHOD) 1 else 0))
    }.toMap ++ routine.body.vars.flatMap(varDec => {
      varDec.varNames.map(varName => {
        counter += 1
        varName -> VariableInfo(varDec.typ, Local, counter - 1)
      })
    }) ++ (if (kind == METHOD) Map(Identifier("this") -> VariableInfo(current, Argument, 0)) else Map())
  }

  def translate(name: Identifier, push: Boolean, classTable: VarTable, routineTable: VarTable): Seq[String] = {
    val variable = if (classTable.contains(name)) classTable(name) else routineTable(name)
    val pushStr = if(push) "push" else "pop"

    Seq(s"$pushStr ${variable.scope.toVM} ${variable.index}")
  }

  case class VariableInfo(typ: LexicalElement, scope: Scope, index: Int)

  trait Scope {
    lazy val toVM = this match {
      case Field => "this"
      case Static => "static"
      case Argument => "argument"
      case Local => "local"
    }
  }
  case object Field extends Scope
  case object Static extends Scope
  case object Argument extends Scope
  case object Local extends Scope
}
