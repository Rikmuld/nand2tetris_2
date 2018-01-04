package nand2tetris.general

import java.io.{File, FileWriter}

import scala.io.Source

object IO {
  type Lines = Seq[String]

  def readFile(location: String): Lines = {
    val inFile = Source.fromFile(location)
    val lines = inFile.getLines.toVector

    inFile.close()
    lines
  }

  def getDirFiles(dir: String): Seq[File] = new File(dir) match {
    case theDir if theDir.exists && theDir.isDirectory =>
      theDir.listFiles
    case _ => Seq()
  }

  def writeFile(location: String, lines: Lines): Unit = {
    val outFile = new java.io.File(location)
    val writer = new FileWriter(outFile)

    outFile.createNewFile()

    if(lines.length > 1) lines.dropRight(1).foreach(l => if (l.length > 0) writer.write(l + "\n"))
    writer.write(lines.last)

    writer.flush()
    writer.close()
  }
}