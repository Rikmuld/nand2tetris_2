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

  def readFiles(dir: String): Map[String, Lines] =
    getDirFiles(dir).map(file => file.getAbsolutePath -> readFile(file.getAbsolutePath)).toMap

  def readFiles(dir: String, typ: String): Map[String, Lines] =
    readFilesFilter(dir)(_.endsWith("." + typ))

  def readFilesFilter(dir: String)(f: String => Boolean): Map[String, Lines] =
    getDirFiles(dir)
      .filter(file => f(file.getAbsolutePath))
      .map(file => file.getAbsolutePath -> readFile(file.getAbsolutePath)).toMap

  def readAll(location: String, typ: String): Map[String, Lines] = location.split("\\.") match {
    case Array(file, ext) if ext == typ =>
      Map(location -> IO.readFile(location))
    case Array(dir) =>
      IO.readFiles(location, typ)
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

  def writeAll(data: Map[String, Lines]): Unit =
    data.foreach {
      case (location, lines) => writeFile(location, lines)
    }

  def readWriteComm[A, B](location: String, readTyp: String, writeTyp: String)
                     (forward: Lines => A)
                     (comm: Seq[A] => B)
                     (backward: (A, B) => Lines): Unit = {
    val data = readAll(location, readTyp) mapValues forward
    val all = comm(data.values.toSeq)
    writeAll(data.map {
      case(name, single) => name.dropRight(readTyp.length) + writeTyp -> backward(single, all)
    })
  }

  def readWrite[A, B](location: String, readTyp: String, writeTyp: String)(map: Lines => Lines): Unit =
    writeAll(readAll(location, readTyp) mapValues map)

  def getDirFiles(dir: String): Seq[File] = new File(dir) match {
    case theDir if theDir.exists && theDir.isDirectory =>
      theDir.listFiles
    case _ => Seq()
  }
}