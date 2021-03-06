package nand2tetris.vmt

import nand2tetris.general.IO._
import nand2tetris.vmt.parser.Parser._
import nand2tetris.vmt.translator.CodeWriter._

object VMTranslator {
  //cleanup with IO object (add combine to IO object)
  def main(args: Array[String]): Unit = args(0).split("\\.") match {
    case Array(file, "vm") =>
      writeFile(file + ".asm", translate(false, readFile(file + ".vm") map parse(file.split("/").last)))
    case Array(dir) =>
      val input = getDirFiles(dir).filter(_.getAbsolutePath.endsWith(".vm"))
      val fileNames = input.map(_.getName.dropRight(3))
      val fileLines = input.map(file => readFile(file.getAbsolutePath))
      val output = translate(true, (fileNames zip fileLines).flatMap(x => x._2 map parse(x._1)))

      writeFile(dir + "/" + dir.split("/").last + ".asm", output)
  }
}