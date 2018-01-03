package nand2tetris.compiler.grammar

import LexicalElements._
import GrammarTree._

trait GrammarTree {
  val xmlName = this match {
    case term: Term => "term"
    case _ => decapitalize(this.getClass.toString.reverse.takeWhile(_ != '.').reverse)
  }
}

object GrammarTree {
  def print(element: GrammarTree)(level: Int = 0): String =
    (" " * level * 2) + s"<${element.xmlName}>\n" + (element match {
      case Class(name, vars, routines) =>
        CLASS.print(level + 1) +
          name.print(level + 1) +
          BRACE_O.print(level + 1) +
          vars.map(print(_)(level + 1)).foldLeft("")(_ + _) +
          routines.map(print(_)(level + 1)).foldLeft("")(_ + _) +
          BRACE_C.print(level + 1)
      case ClassVariable(kind, typ, varNames) =>
        kind.print(level + 1) +
          typ.print(level + 1) +
          printSpacedList(COMMA, varNames, level + 1) +
          SEMICOLON.print(level + 1)
      case ClassRoutine(kind, returnType, name, params, body) =>
        kind.print(level + 1) +
          returnType.print(level + 1) +
          name.print(level + 1) +
          PARENTHESIS_O.print(level + 1) +
          printSpacedList(COMMA, params, level + 1)
          PARENTHESIS_C.print(level + 1) +
          print(body)(level + 1)
      case Parameter(typ, name) =>
        typ.print(level + 1) +
          name.print(level + 1)
      case SubroutineBody(variables, statements) =>
        BRACE_O.print(level + 1) +
        variables.map(print(_)(level + 1)).foldLeft("")(_ + _) +
          statements.map(print(_)(level + 1)).foldLeft("")(_ + _)+
        BRACE_C.print(level + 1)
      case VarDec(typ, varNames) =>
        VAR.print(level + 1) +
          typ.print(level + 1) +
          printSpacedList(COMMA, varNames, level + 1)
          SEMICOLON.print(level + 1)
      case LetStatement(varName, optionalArray, equals) =>
        LET.print(level + 1) +
          varName.print(level + 1) +
          optionalArray.fold("")(a =>
            BRACKET_SQ_O.print(level + 1) +
              print(a)(level + 1) +
              BRACKET_SQ_C.print(level + 1)
          ) +
          EQUALS.print(level + 1) +
          print(equals)(level + 1) +
          SEMICOLON.print(level + 1)
      case DoStatement(call) =>
        DO.print(level + 1) +
          print(call)(level + 1) +
          SEMICOLON.print(level + 1)
      case WhileStatement(condition, whileTrue) =>
        WHILE.print(level + 1) +
          PARENTHESIS_O.print(level + 1) +
          print(condition)(level + 1) +
          PARENTHESIS_C.print(level + 1) +
          BRACE_O.print(level + 1) +
          whileTrue.map(print(_)(level + 1)).foldLeft("")(_ + _) +
          BRACE_C.print(level + 1)
      case IfStatement(condition, ifTrue, ifFalse) =>
        IF.print(level + 1) +
          PARENTHESIS_O.print(level + 1) +
          print(condition)(level + 1) +
          PARENTHESIS_C.print(level + 1) +
          BRACE_O.print(level + 1)
        ifTrue.map(print(_)(level + 1)).foldLeft("")(_ + _) +
          BRACE_C.print(level + 1) +
          ifFalse.fold("")(s =>
            ELSE.print(level + 1) +
              BRACE_O.print(level + 1) +
              s.map(print(_)(level + 1)).foldLeft("")(_ + _) +
              BRACE_C.print(level + 1)
          )
      case ReturnStatement(optionalReturn) =>
        RETURN.print(level + 1) +
          optionalReturn.fold("")(print(_)(level + 1)) +
          SEMICOLON.print(level + 1)
      case Expression(term, opTerm) =>
        print(term)(level + 1) +
          opTerm.map(data => data._1.print(level + 1) + print(data._2)(level + 1)).foldLeft("")(_ + _)
      case ExpressionTerm(exp) =>
        PARENTHESIS_O.print(level + 1) +
          print(exp)(level + 1) +
          PARENTHESIS_C.print(level + 1)
      case SubroutineCall(prefix, routineName, callParameters) =>
        prefix.fold("")(e =>
          e.print(level + 1) +
            POINT.print(level + 1)
        ) +
          routineName.print(level + 1) +
          PARENTHESIS_O.print(level + 1) +
          printSpacedList(COMMA, callParameters, level + 1)
          PARENTHESIS_C.print(level + 1)
      case SimpleTerm(lexical) =>
        lexical.print(level + 1)
      case ArrayTerm(varName, arrayExp) =>
        varName.print(level + 1) +
          BRACKET_SQ_O.print(level + 1) +
          print(arrayExp)(level + 1) +
          BRACKET_SQ_C.print(level + 1)
      case UnitaryOpTerm(op, term) =>
        op.print(level + 1) +
          print(term)(level + 1)
      case _ =>
        ""
    }) + (" " * level * 2) + s"</${element.xmlName}>\n"

  def printSpacedList[A <: LexicalElement](splitter: LexicalElement, list: Seq[A], level: Int): String =
    if(list.isEmpty) ""
    else list.head.print(level) + list.tail.map(_.print(level)).foldLeft("")(_ + splitter.print(level) + _)

  def printSpacedList[A <: GrammarTree](splitter: LexicalElement, list: Seq[A], level: Int): String =
    if(list.isEmpty) ""
    else print(list.head)(level) + list.tail.map(print(_)(level)).foldLeft("")(_ + splitter.print(level) + _)

  def decapitalize(str: String): String =
    str.charAt(0).toLower.toString + str.substring(1)
}

//TODO handle lists correctly
//TODO spaced match all add (i.e. commas)