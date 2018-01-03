package nand2tetris.compiler.parsers

import nand2tetris.compiler.grammar._
import LexicalElements._
import Parser._
import StatementParser._
import nand2tetris.general.State._

object ClassParser {
  val matchVariableType =
    matchEither(matchAny(INT, CHAR, BOOLEAN), matchIdentifier)

  val matchResultType =
    matchEither(matchAny(INT, CHAR, BOOLEAN, VOID), matchIdentifier)

  def parseClass: Parser[Class] = rewind(for {
    _ <- matchA(CLASS)
    className <- matchIdentifier
    _ <- matchA(BRACE_O)
    variables <- parseAll(parseClassVariable)
    routines <- parseAll(parseClassRoutine)
    _ <- matchA(BRACE_C)
  } yield Class(className, variables, routines))

  def parseClassVariable: Parser[ClassVarDec] = rewind(for {
    kind <- matchAny(STATIC, FIELD)
    typ <- matchVariableType
    variables <- parseAllSpaced(COMMA)(matchIdentifier)
    _ <- matchA(SEMICOLON)
  } yield ClassVarDec(kind, typ, variables))

  def parseClassRoutine: Parser[SubroutineDec] = rewind(for {
    kind <- matchAny(CONSTRUCTOR, FUNCTION, METHOD)
    typ <- matchResultType
    name <- matchIdentifier
    _ <- matchA(PARENTHESIS_O)
    parameters <- parseAllSpaced(COMMA)(parseParameter)
    _ <- matchA(PARENTHESIS_C)
    _ <- matchA(BRACE_O)
    body <- parseRoutineBody
    _ <- matchA(BRACE_C)
  } yield SubroutineDec(kind, typ, name, parameters, body))

  def parseParameter: Parser[Parameter] = rewind(for {
    typ <- matchVariableType
    name <- matchIdentifier
  } yield Parameter(typ, name))

  def parseRoutineBody: Parser[SubroutineBody] = rewind(for {
    variables <- parseAll(parseRoutineVariable)
    statements <- parseAll(parseStatement)
  } yield SubroutineBody(variables, statements))

  def parseRoutineVariable: Parser[VarDec] = rewind(for {
    _ <- matchA(VAR)
    typ <- matchVariableType
    variables <- parseAllSpaced(COMMA)(matchIdentifier)
    _ <- matchA(SEMICOLON)
  } yield VarDec(typ, variables))
}