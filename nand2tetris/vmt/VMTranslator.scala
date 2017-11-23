package nand2tetris.vmt

import nand2tetris.general.IO._
import nand2tetris.vmt.parser.Parser._
import nand2tetris.vmt.translator.CodeWriter._

object VMTranslator {
  var filename: String = _

  def main(args: Array[String]): Unit = args(0).split("\\.") match {
    case Array(file, "vm") =>
      filename = file
      writeFile(file + ".asm", translate(readFile(file + ".vm") map parse))
  }
}