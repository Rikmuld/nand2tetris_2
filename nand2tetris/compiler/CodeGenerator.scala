package nand2tetris.compiler

import nand2tetris.general.IO
import JackAnalyzer._
import Tokenizer._
import nand2tetris.compiler.writer.ClassTranslator._

object CodeGenerator {
  def main(args: Array[String]): Unit =
    IO.readWriteComm(args(0), "jack", "vm")(tokenize _ andThen analyzeFile _)(argCount)(translate)
}