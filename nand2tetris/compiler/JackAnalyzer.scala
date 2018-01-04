package nand2tetris.compiler

import nand2tetris.compiler.parsers.ClassParser._
import nand2tetris.compiler.grammar._
import nand2tetris.general.IO._

object JackAnalyzer {
  def main(args: Array[String]): Unit = args(0).split("\\.") match {
    case Array(file, "jack") =>
      writeFile(file + ".xml", Seq(GrammarTree.print(parseClass.run(Tokenizer.tokenize(readFile(file + ".jack")))._1.get)(true, 0)))
    case Array(dir) =>
      getDirFiles(dir).filter(_.getAbsolutePath.endsWith(".jack")) foreach { file =>
        writeFile(file.getAbsolutePath.dropRight(5) + ".xml", Seq(GrammarTree.print(parseClass.run(Tokenizer.tokenize(readFile(file.getAbsolutePath)))._1.get)(true, 0)))
      }
  }
}