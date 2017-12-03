package nand2tetris.vmt.translator

import nand2tetris.vmt.parser.Stack._

import nand2tetris.vmt.translator.TranslationHelper.Stack._
import nand2tetris.vmt.translator.TranslationHelper.Register._
import nand2tetris.vmt.translator.TranslationHelper.Segment._
import nand2tetris.vmt.translator.TranslationHelper._
import nand2tetris.vmt.translator.CompPreamble._

object CodeWriter {
  val functionEnd: Seq[String] =
    pop ++ goto(ARGUMENT) ++ setAt ++
      goto(ARGUMENT) ++ get ++ goto(SP) ++ Seq("M=D+1") ++
      goto(LOCAL) ++ get ++ sub(5) ++ getAtD ++ setReg(1) ++
      goto(LOCAL) ++ Seq("A=M-1") ++ get ++ goto(THAT) ++ set ++
      goto(LOCAL) ++ get ++ sub(2) ++ getAtD ++ goto(THIS) ++ set ++
      goto(LOCAL) ++ get ++ sub(3) ++ getAtD ++ goto(ARGUMENT) ++ set ++
      goto(LOCAL) ++ get ++ sub(4) ++ getAtD ++ goto(LOCAL) ++ set ++
      atReg(1) ++ jump

  def translate(stack: Stack): Seq[String] =
    compPreamble(stack) ++ stack.zipWithIndex.flatMap {
      case (line, i) => translate(line, i)
    }

  def translate(line: StackLine, i: Int): Seq[String] = line match {
    case Op(op) => translateLogic(op, i)
    case Memory(action, segment, j) => translateSegment(action, segment, j)
    case Label(lab) => label(lab)
    case Jump(lab, condition) => translateJump(lab, condition)
    case Function(name, nArgs) => functionBegin(name, nArgs, i)
    case FunctionCall(name, nArgs) => functionCall(name, nArgs, i)
    case Return => functionEnd
    case Fluff => Nil
  }

  def functionBegin(name: String, nArgs: Int, i:Int): Seq[String] =
    label(s"function.$name")++ goto(SP) ++ get ++ goto(LOCAL) ++ set ++ List.fill(nArgs)(pushZero).flatten

  def functionCall(name: String, nArgs: Int, i:Int): Seq[String] =
    goto(s"function.$name.return.$i") ++ getAddress ++ push ++
      goto(LOCAL) ++ get ++ push ++
      goto(ARGUMENT) ++ get ++ push ++
      goto(THIS) ++ get ++ push ++
      goto(THAT) ++ get ++ push ++
      goto(SP) ++ get ++ sub(nArgs + 5) ++ goto(ARGUMENT) ++ set ++
      goto(s"function.$name") ++ jump ++ label(s"function.$name.return.$i")

  def translateJump(lab: String, condition: Boolean): Seq[String] = condition match {
    case false => goto(lab) ++ jump
    case true => pop ++ goto(lab) ++ jumpNEQ
  }

  def translateLogic(line: StackOps, i: Int): Seq[String] = line match {
    case Unitary(op) => mutate(op)
    case Arithmetic(op) => pop ++ setReg(0) ++ pop ++ mutateReg(0, "D" + op + "M") ++ getReg(0) ++ push
    case Compare(jmp) => pop ++ setReg(0) ++ pop ++ mutateReg(0, "D-M") ++ comp(jmp, i.toString) ++ push
  }

  def translateSegment(pushAction: Boolean, segment: Segment, i: Int): Seq[String] =
    (segment, pushAction) match {
      case (Relative(pointer), false) => pop ++ setToSeg(pointer, i)
      case (Relative(pointer), true) => getFromSeg(pointer, i) ++ push
      case (Constant, true) => constant(i) ++ push
      case (Absolute(f), false) => pop ++ setTo(f(i))
      case (Absolute(f), true) => getFrom(f(i)) ++ push
    }
}
