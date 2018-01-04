package nand2tetris.compiler.writer

import nand2tetris.compiler.grammar._
import nand2tetris.compiler.writer.VariableAnalyzer.{Static, VarTable}
import nand2tetris.compiler.grammar.LexicalElements._
import TranslationHelper._

object ClassTranslator {
  type ArgMap = Map[String, Int]

  def argCount(clsCount: Seq[Class]): ArgMap =
    ???

  def translte(cls: Class, argCount: ArgMap): Seq[String] =
    cls.fs.flatMap(routine => translate(routine, cls.name, VariableAnalyzer.createClassTable(cls.vars), argCount))

  private def translate(routine: SubroutineDec, current: Identifier, cv: VarTable, argCount: ArgMap): Seq[String] = {
    val rv = VariableAnalyzer.createRoutineTable(routine, current, routine.kind)

    Seq(s"function $current.${routine.name}") ++ (routine.kind match {
      case CONSTRUCTOR =>
        constant(cv.count(_._2.scope == FIELD)) ++
        Seq("call Memory.alloc 1") ++
        Seq("pop pointer 0")
      case METHOD =>
        Seq("push argument 0")
        Seq("pop pointer 0")
      case FUNCTION =>
        Seq()
    }) ++ routine.body.statements.flatMap(s =>
      StatementTranslator.translate(s, current, if(routine.kind == FUNCTION) cv.filter(_._2.scope == Static) else cv, rv, argCount)
    )
  }
}