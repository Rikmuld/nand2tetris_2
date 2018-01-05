package nand2tetris.compiler.writer

import nand2tetris.compiler.grammar._
import nand2tetris.compiler.writer.VariableAnalyzer.{Field, Static, VarTable}
import nand2tetris.compiler.grammar.LexicalElements._
import TranslationHelper._

object ClassTranslator {
  type ArgMap = Map[String, Int]

  val argCountOS = Map(
    "Math.multiply" -> 2,
    "Math.divide" -> 2,
    "Math.min" -> 2,
    "Math.max" -> 2,
    "Math.sqrt" -> 1,
    "Math.abs" -> 1,
    "String.new" -> 1,
    "String.dispose" -> 0,
    "String.length" -> 0,
    "String.setCharAt" -> 1,
    "String.charAt" -> 2,
    "String.appendChar" -> 1,
    "String.eraseLastChar" -> 0,
    "String.intValue" -> 0,
    "String.setInt" -> 1,
    "String.backSpace" -> 0,
    "String.doubleQuote" -> 0,
    "String.newLine" -> 0,
    "Array.new" -> 1,
    "Array.dispose" -> 0,
    "Output.moveCursor" -> 2,
    "Output.printChar" -> 1,
    "Output.printString" -> 1,
    "Output.printInt" -> 1,
    "Output.println" -> 0,
    "Output.backSpace" -> 0,
    "Screen.clearScreen" -> 0,
    "Screen.setColor" -> 1,
    "Screen.drawPixel" -> 2,
    "Screen.drawLine" -> 4,
    "Screen.drawRectangle" -> 4,
    "Screen.drawCircle" -> 3,
    "Keyboard.keyPressed" -> 0,
    "Keyboard.readChar" -> 0,
    "Keyboard.readLine" -> 1,
    "Keyboard.readInt" -> 1,
    "Memory.peek" -> 1,
    "Memory.poke" -> 2,
    "Memory.alloc" -> 1,
    "Memory.deAlloc" -> 1,
    "Sys.halt" -> 0,
    "Sys.error" -> 1,
    "Sys.wait" -> 1
  )

  def argCount(classes: Seq[Class]): ArgMap =
    classes.flatMap(cls =>
      cls.fs.map(routine =>
        (cls.name.str + "." + routine.name.str) -> routine.params.size
      )
    ).toMap ++ argCountOS

  def translate(cls: Class, argCount: ArgMap): Seq[String] =
    cls.fs.flatMap(routine => translate(routine, cls.name, VariableAnalyzer.createClassTable(cls.vars), argCount))

  private def translate(routine: SubroutineDec, current: Identifier, cv: VarTable, argCount: ArgMap): Seq[String] = {
    val rv = VariableAnalyzer.createRoutineTable(routine, current, routine.kind)

    Seq(s"function ${current.str}.${routine.name.str} ${routine.body.vars.foldLeft(0)(_ + _.varNames.size)}") ++ (routine.kind match {
      case CONSTRUCTOR =>
        constant(cv.count(_._2.scope == Field)) ++
        Seq("call Memory.alloc 1") ++
        Seq("pop pointer 0")
      case METHOD =>
        Seq("push argument 0") ++
        Seq("pop pointer 0")
      case FUNCTION =>
        Seq()
    }) ++ routine.body.statements.flatMap(s =>
      StatementTranslator.translate(s, current, if(routine.kind == FUNCTION) cv.filter(_._2.scope == Static) else cv, rv, argCount)
    )
  }
}