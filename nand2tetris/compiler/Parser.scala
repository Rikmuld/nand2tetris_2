package nand2tetris.compiler

import nand2tetris.compiler.Tokenizer.Tokens
import nand2tetris.compiler.parsers.ClassParser._
import nand2tetris.compiler.grammar._
import nand2tetris.general.IO

object Parser {
  def parse(a: Tokens): Option[Class] =
    parseClass.run(a)._1

  def main(args: Array[String]): Unit =
    println(GrammarTree.print(parse(Tokenizer.tokenize(IO.readFile(args(0)))).get)(true, 0))
}
