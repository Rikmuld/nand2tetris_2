package nand2tetris.vmt.translator

import TranslationHelper.Register._

object TranslationHelper {
  final val SP = "SP"
  final val THAT = "THAT"
  final val THIS = "THIS"
  final val LOCAL = "LCL"
  final val ARGUMENT = "ARG"

  final val TEMP_OFFSET = 5

  final val REGISTER_1 = 13
  final val REGISTER_2 = 14
  final val REGISTER_3 = 15

  val set =
    Seq("M=D")

  val get =
    Seq("D=M")

  val getAddress =
    Seq("D=A")

  val at =
    Seq("A=M")

  val atD =
    Seq("A=D")

  val setAt =
    at ++ set

  val getAt =
    at ++ get

  val getAtD =
    atD ++ get

  val setZero =
    Seq("M=0")

  val setZeroAt =
    at ++ setZero

  val jump =
    Seq("0;JMP")

  val setFalse =
    Seq("D=0")

  val setTrue =
    Seq("D=-1")

  val jumpLT =
    Seq("D;JLT")

  val jumpGT =
    Seq("D;JGT")

  val jumpEQ =
    Seq("D;JEQ")

  val jumpNEQ =
    Seq("D;JNE")

  def label(label: String) =
    Seq(s"($label)")

  def goto(a: String) =
    Seq(s"@$a")

  def goto(a: Int) =
    Seq(s"@$a")

  def constant(i: Int) =
    goto(i) ++ getAddress

  def comp(jmp: String, id: String) =
    (goto(s"COMP.$id") :+ "D=A") ++ goto(s"COMP.$jmp") ++ jump :+ s"(COMP.$id)"

  def setTo(index: Int) =
    goto(index) ++ set

  def setTo(label: String) =
    goto(label) ++ set

  def getFrom(index: Int) =
    goto(index) ++ get

  def getFrom(label: String) =
    goto(label) ++ get

  def add(n:Int) =
    setReg(0) ++ constant(n) ++ gotoReg(0) :+ "D=M+D"

  def sub(n:Int) =
    setReg(0) ++ constant(n) ++ gotoReg(0) :+ "D=M-D"

  object Stack {
    val pop =
      (goto(SP) :+ "M=M-1") ++ getAt

    val push =
      goto(SP) ++ setAt ++ goto(SP) :+ "M=M+1"

    val pushZero =
      goto(SP) ++ setZeroAt ++ goto(SP) :+ "M=M+1"

    def mutate(op: String) =
      ((goto(SP) :+ "M=M-1") ++ at :+ s"M=${op}M") ++ goto(SP) :+ "M=M+1"
  }

  object Register {
    def gotoReg(register: Int) =
      goto((REGISTER_1 + register).toString)

    def setReg(register: Int) =
      gotoReg(register) ++ set

    def getReg(register: Int) =
      gotoReg(register) ++ get

    def atReg(register: Int) =
      gotoReg(register) ++ at

    def setAtReg(register: Int) =
      atReg(register) ++ set

    def getAtReg(register: Int) =
      atReg(register) ++ get

    def mutateReg(register: Int, act: String) =
      gotoReg(register) :+ s"M=$act"
  }

  object Segment {
    def getOffset(segment: String, i: Int) =
      constant(i) ++ goto(segment) :+ "D=M+D"

    def setToSeg(segment: String, i: Int) =
      setReg(0) ++ getOffset(segment, i) ++ setReg(1) ++ getReg(0) ++ setAtReg(1)

    def getFromSeg(segment: String, i: Int) =
      (getOffset(segment, i) :+ "A=D") ++ get
  }
}