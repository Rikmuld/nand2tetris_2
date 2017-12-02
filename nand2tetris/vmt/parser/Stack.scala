package nand2tetris.vmt.parser

import nand2tetris.vmt.VMTranslator._
import nand2tetris.vmt.translator.TranslationHelper._

object Stack {
  type Stack = Seq[StackLine]

  trait StackLine
  case class Memory(push: Boolean, segment: Segment, i: Int) extends StackLine
  case class Op(op: StackOps) extends StackLine
  case class Label(label: String) extends StackLine
  case class Jump(label: String, condition: Boolean) extends StackLine
  case object Fluff extends StackLine

  trait StackOps
  case class Unitary(operation: String) extends StackOps
  case class Arithmetic(op: String) extends StackOps
  case class Compare(con: String) extends StackOps

  implicit def opsFromString(str: String): StackOps = str match {
    case "add" => Arithmetic("+")
    case "sub" => Arithmetic("-")
    case "neg" => Unitary("-")
    case "eq" => Compare("JEQ")
    case "gt" => Compare("JGT")
    case "lt" => Compare("JLT")
    case "and" => Arithmetic("&")
    case "or" => Arithmetic("|")
    case "not" => Unitary("!")
  }

  trait Segment
  case class Relative(pointer: String) extends Segment
  case class Absolute(f: Int => String) extends Segment
  case object Constant extends Segment

  implicit def segmentFromString(str: String): Segment = str match {
    case "local" => Relative(LOCAL)
    case "argument" => Relative(ARGUMENT)
    case "this" => Relative(THIS)
    case "that" => Relative(THAT)
    case "constant" => Constant
    case "static" => Absolute(filename + "." + _)
    case "temp" => Absolute(i => (TEMP_OFFSET + i).toString)
    case "pointer" => Absolute(i => if(i == 0) THIS else THAT)
  }
}