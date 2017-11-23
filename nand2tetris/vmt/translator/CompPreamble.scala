package nand2tetris.vmt.translator

import nand2tetris.vmt.parser.Stack._
import nand2tetris.vmt.translator.TranslationHelper._

object CompPreamble {
  type CompExist = (Boolean, Boolean, Boolean)

  private val mainStart =
    label("MAIN")

  private val mainJump =
    goto("MAIN") ++ jump

  private val compIfDone =
    label("COMP.IF") ++ setTrue ++ label("COMP.DONE") ++ goto(REGISTER_2) ++ at ++ jump

  private def comp(comp: String) =
    (label(s"COMP.$comp") ++ setTo(REGISTER_2) ++ getFrom(REGISTER_1) ++
      goto("COMP.IF") :+ s"D;$comp") ++ setFalse ++ goto("COMP.DONE") ++ jump

  private def compExists(stack: Stack): CompExist = stack.foldLeft((false, false, false)){
    case ((eq, lt, gt), next) => next match {
      case Op(Compare("JEQ")) => (true, lt, gt)
      case Op(Compare("JLT")) => (eq, true, gt)
      case Op(Compare("JGT")) => (eq, lt, true)
      case _ => (eq, lt, gt)
    }
  }

  def compPreamble(stack: Stack): Seq[String] = compExists(stack) match {
    case ((false, false, false)) => Nil
    case ((eq, lt, gt)) =>
      val comp =
        (if(eq) CompPreamble.comp("JEQ") else Nil) ++
          (if(lt) CompPreamble.comp("JLT") else Nil) ++
          (if(gt) CompPreamble.comp("JGT") else Nil)

      CompPreamble.mainJump ++ comp ++ CompPreamble.compIfDone ++ CompPreamble.mainStart
  }
}