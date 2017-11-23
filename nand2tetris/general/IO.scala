package nand2tetris.general

import java.io.FileWriter

import scala.io.Source

object IO {
  type Lines = Seq[String]

  def readFile(location: String): Lines = {
    val inFile = Source.fromFile(location)
    val lines = inFile.getLines.toVector

    inFile.close()
    lines
  }

  def writeFile(location: String, lines: Lines): Unit = {
    val outFile = new java.io.File(location)
    val writer = new FileWriter(outFile)

    outFile.createNewFile()
    lines.foreach(l => if (l.length > 0) writer.write(l + "\n"))

    writer.flush()
    writer.close()
  }
}