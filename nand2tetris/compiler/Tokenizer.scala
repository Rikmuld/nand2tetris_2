package nand2tetris.compiler

import nand2tetris.compiler.grammar.LexicalElements
import nand2tetris.compiler.grammar.LexicalElements._
import nand2tetris.general.IO.Lines

import scala.util.{Failure, Success, Try}

object Tokenizer {
  type Tokens = Seq[LexicalElement]

  def tokenize(file: Lines): Tokens =
    tokenize(cleanLines(file))

  private def tokenize(str: String): Tokens = {
    def run(str: String, acc: Tokens = Seq()): Tokens =
      if(str.length == 0) acc
      else {
        val char = str.charAt(0)

        if(char == '"') {
          val string = str.substring(1).takeWhile(_ != '"')
          run(str.substring(string.length + 2).trim, acc :+ ConstantString(string))
        }
        else if(isSymbol(char)) run(str.substring(1).trim, acc :+ symbolFromChar(char).get)
        else {
          val word = str.takeWhile(c => !(c == ' ' || isSymbol(c)))
          run(str.substring(word.length).trim, acc :+ tokenizeWord(word))
        }
      }

    run(str.trim)
  }

  def cleanLines(file: Lines): String =
    cleanSpaces(stripMultiComments((file map stripComment).foldLeft("")(_ + _)))

  def stripComment(str: String): String =
    str.replaceAll("//.+", "")

  def stripMultiComments(str: String): String =
    str.replaceAll("/\\*\\*.+?\\*/", "")

  def cleanSpaces(str: String): String =
    str.replaceAll("\\s+", " ")

  def tokenizeWord(str: String): LexicalElement =
    if(str.substring(0, 1).matches("\\d")) Try(str.toInt) match {
      case Success(i) => ConstantInteger(i)
      case Failure(err) => Error("Non integer constants cannot start with a digit: '" + str + "'")
    } else keywordFromString(str) match {
      case Some(keyword) => keyword
      case None if isIdentifier(str) => Identifier(str)
      case _ => Error("Could not tokenize: '" + str + "'")
    }
}