package nand2tetris.compiler

import nand2tetris.compiler.Tokenizer.Tokens
import nand2tetris.compiler.parsers.ClassParser._
import nand2tetris.compiler.grammar._
import nand2tetris.general.IO._
import Tokenizer._
import nand2tetris.compiler.grammar.GrammarTree._

object JackAnalyzer {
  def main(args: Array[String]): Unit =
    readWrite(args(0), "jack", ".xml")(
      tokenize _ andThen analyzeFile _ andThen printTree _ andThen toSeq _
    )

  def analyzeFile(tokens: Tokens): Class =
    parseClass.run(tokens)._1.get

  private def toSeq(str: String): Seq[String] =
    Seq(str)
}