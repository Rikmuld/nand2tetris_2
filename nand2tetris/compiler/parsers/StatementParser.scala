package nand2tetris.compiler.parsers

import nand2tetris.compiler.grammar._
import LexicalElements._
import Parser._
import nand2tetris.general.State
import State._
import ExpressionParser._

object StatementParser {
  val statementsBetween =
    between(BRACE_O, BRACE_C)(parseAll(parseStatement))

  val condition =
    between(PARENTHESIS_O, PARENTHESIS_C)(parseExpression)

  def parseStatement: Parser[Statement] =
    tryAll[Statement](parseIf, parseDo, parseLet, parseWhile, parseReturn)

  def parseIf: Parser[Statement] = rewind(for {
    _ <- matchA(IF)
    condition <- condition
    iff <- statementsBetween
    els <- optional(prefix(ELSE)(statementsBetween))
  } yield IfStatement(condition, iff, els))

  def parseDo: Parser[Statement] = rewind(for {
    _ <- matchA(DO)
    call <- suffix(SEMICOLON)(parseRoutineCall)
  } yield DoStatement(call))

  def parseLet: Parser[Statement] = rewind(for {
    _ <- matchA(LET)
    name <- matchIdentifier
    arrayExpression <- optional(between(BRACKET_SQ_O, BRACKET_SQ_C)(parseExpression))
    _ <- matchA(EQUALS)
    equalsExpression <- suffix(SEMICOLON)(parseExpression)
  } yield LetStatement(name, arrayExpression, equalsExpression))

  def parseWhile: Parser[Statement] = rewind(for {
    _ <- matchA(WHILE)
    condition <- condition
    iff <- statementsBetween
  } yield WhileStatement(condition, iff))

  def parseReturn: Parser[Statement] = rewind(for {
    _ <- matchA(RETURN)
    expression <- suffix(SEMICOLON)(optional(parseExpression))
  } yield ReturnStatement(expression))
}
