package nand2tetris.compiler.parsers

import nand2tetris.compiler.grammar._
import LexicalElements._
import nand2tetris.compiler.parsers.Parser.Parser
import Parser._
import nand2tetris.general.State._

object ExpressionParser {
  val matchOp = matchAny(PLUS, MINUS, ASTERISK, SLASH, AND, OR, BRACKET_AG_C, BRACKET_AG_O, EQUALS)
  val matchUniOp = matchAny(MINUS, NOT)
  val matchKeywordConstant = matchAny(TRUE, FALSE, NULL, THIS)

  def parseExpression: Parser[Expression] = rewind(for {
    term <- parseTerm
    opTermList <- parseAll(for {
      op <- matchOp
      term <- parseTerm
    } yield (op, term))
  } yield Expression(term, opTermList))

  def parseRoutineCall: Parser[SubroutineCall] = rewind(for {
    prefix <- optional(suffix(POINT)(matchIdentifier))
    routineName <- matchIdentifier
    expressions <- between(PARENTHESIS_O, PARENTHESIS_C)(parseAllSpaced(COMMA)(parseExpression))
  } yield SubroutineCall(prefix, routineName, expressions))

  def parseTerm: Parser[Term] =
    tryAll[Term](
      parseUnitaryTerm,
      parseArrayTerm,
      parseRoutineCall.asInstanceOf[Parser[Term]],
      parseExpressionTerm.asInstanceOf[Parser[Term]],
      parseSimpleTerm)

  def parseExpressionTerm: Parser[ExpressionTerm] =
     rewind(between(PARENTHESIS_O, PARENTHESIS_C)(parseExpression).map(a => ExpressionTerm(a)))

  def parseUnitaryTerm: Parser[Term] = rewind(for {
    op <- matchUniOp
    term <- parseTerm
  } yield UnitaryOpTerm(op, term))

  def parseArrayTerm: Parser[Term] = rewind(for {
    name <- matchIdentifier
    expression <- between(BRACKET_SQ_O, BRACKET_SQ_C)(parseExpression)
  } yield ArrayTerm(name, expression))

  def parseSimpleTerm: Parser[Term] =
    tryAll[LexicalElement](
      matchInteger.asInstanceOf[Parser[LexicalElement]],
      matchString.asInstanceOf[Parser[LexicalElement]],
      matchKeywordConstant.asInstanceOf[Parser[LexicalElement]],
      matchIdentifier.asInstanceOf[Parser[LexicalElement]]).map((lex:LexicalElement) => {
      SimpleTerm(lex)
    })
}
