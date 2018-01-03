package nand2tetris.compiler.grammar

import LexicalElements._
import GrammarTree._

trait GrammarTree {
  val xmlName = this match {
    case term: Term => "term"
    case _ => decapitalize(this.getClass.toString.reverse.takeWhile(_ != '.').reverse)
  }

  val forceName = this match {
    case call: SubroutineCall => false
    case _ => true
  }
}

object GrammarTree {
  def print(element: GrammarTree)(printName: Boolean = true, level: Int = 0): String =
    encapsulate(element.xmlName, printName && element.forceName, level, element match {
      case Class(name, vars, routines) =>
        CLASS.print(level + 1) +
          name.print(level + 1) +
          BRACE_O.print(level + 1) +
          vars.map(print(_)(true, level + 1)).foldLeft("")(_ + _) +
          routines.map(print(_)(true, level + 1)).foldLeft("")(_ + _) +
          BRACE_C.print(level + 1)
      case ClassVarDec(kind, typ, varNames) =>
        kind.print(level + 1) +
          typ.print(level + 1) +
          printSpacedList1(COMMA, varNames, level + 1) +
          SEMICOLON.print(level + 1)
      case SubroutineDec(kind, returnType, name, params, body) =>
        kind.print(level + 1) +
          returnType.print(level + 1) +
          name.print(level + 1) +
          PARENTHESIS_O.print(level + 1) +
          encapsulate("parameterList", true, level + 1, printSpacedList2(COMMA, params, false, level + 2)) +
          PARENTHESIS_C.print(level + 1) +
          print(body)(true, level + 1)
      case Parameter(typ, name) =>
        typ.print(level + 1) +
          name.print(level + 1)
      case SubroutineBody(variables, statements) =>
        BRACE_O.print(level + 1) +
          variables.map(print(_)(true, level + 1)).foldLeft("")(_ + _) +
          encapsulate("statements", statements.nonEmpty, level + 1, statements.map(print(_)(true, level + 2)).foldLeft("")(_ + _)) +
          BRACE_C.print(level + 1)
      case VarDec(typ, varNames) =>
        VAR.print(level + 1) +
          typ.print(level + 1) +
          printSpacedList1(COMMA, varNames, level + 1) +
          SEMICOLON.print(level + 1)
      case LetStatement(varName, optionalArray, equals) =>
        LET.print(level + 1) +
          varName.print(level + 1) +
          optionalArray.fold("")(a =>
            BRACKET_SQ_O.print(level + 1) +
              print(a)(true, level + 1) +
              BRACKET_SQ_C.print(level + 1)
          ) +
          EQUALS.print(level + 1) +
          print(equals)(true, level + 1) +
          SEMICOLON.print(level + 1)
      case DoStatement(call) =>
        DO.print(level + 1) +
          print(call)(true, level + 1) +
          SEMICOLON.print(level + 1)
      case WhileStatement(condition, whileTrue) =>
        WHILE.print(level + 1) +
          PARENTHESIS_O.print(level + 1) +
          print(condition)(true, level + 1) +
          PARENTHESIS_C.print(level + 1) +
          BRACE_O.print(level + 1) +
          encapsulate("statements", whileTrue.nonEmpty, level + 1, whileTrue.map(print(_)(true, level + 2)).foldLeft("")(_ + _)) +
          BRACE_C.print(level + 1)
      case IfStatement(condition, ifTrue, ifFalse) =>
        IF.print(level + 1) +
          PARENTHESIS_O.print(level + 1) +
          print(condition)(true, level + 1) +
          PARENTHESIS_C.print(level + 1) +
          BRACE_O.print(level + 1) +
        encapsulate("statements", ifTrue.nonEmpty, level + 1, ifTrue.map(print(_)(true, level + 2)).foldLeft("")(_ + _)) +
          BRACE_C.print(level + 1) +
          ifFalse.fold("")(s =>
            ELSE.print(level + 1) +
              BRACE_O.print(level + 1) +
              encapsulate("statements", s.nonEmpty, level + 1, s.map(print(_)(true, level + 2)).foldLeft("")(_ + _)) +
              BRACE_C.print(level + 1)
          )
      case ReturnStatement(optionalReturn) =>
        RETURN.print(level + 1) +
          optionalReturn.fold("")(print(_)(true, level + 1)) +
          SEMICOLON.print(level + 1)
      case Expression(term, opTerm) =>
        print(term)(true, level + 1) +
          opTerm.map(data => data._1.print(level + 1) + print(data._2)(true, level + 1)).foldLeft("")(_ + _)
      case ExpressionTerm(exp) =>
        PARENTHESIS_O.print(level + 1) +
          print(exp)(true, level + 1) +
          PARENTHESIS_C.print(level + 1)
      case SubroutineCallTerm(routine) =>
          print(routine)(true, level + 1)
      case SubroutineCall(prefix, routineName, callParameters) =>
        prefix.fold("")(e =>
          e.print(level) +
            POINT.print(level)
        ) +
          routineName.print(level) +
          PARENTHESIS_O.print(level) +
          encapsulate("expressionList", true, level, printSpacedList2(COMMA, callParameters, true, level + 1)) +
          PARENTHESIS_C.print(level)
      case SimpleTerm(lexical) =>
        lexical.print(level + 1)
      case ArrayTerm(varName, arrayExp) =>
        varName.print(level + 1) +
          BRACKET_SQ_O.print(level + 1) +
          print(arrayExp)(true, level + 1) +
          BRACKET_SQ_C.print(level + 1)
      case UnitaryOpTerm(op, term) =>
        op.print(level + 1) +
          print(term)(true, level + 1)
      case _ =>
        ""
    })

  def encapsulate(name: String, printName: Boolean, level: Int, string: String): String =
    (if (printName) (" " * level * 2) + s"<${name}>\n" else "") +
      string +
      (if (printName) (" " * level * 2) + s"</${name}>\n" else "")

  def printSpacedList1[A <: LexicalElement](splitter: LexicalElement, list: Seq[A], level: Int): String =
    if (list.isEmpty) ""
    else list.head.print(level) + list.tail.map(_.print(level)).foldLeft("")(_ + splitter.print(level) + _)

  def printSpacedList2[A <: GrammarTree](splitter: LexicalElement, list: Seq[A], doPrint: Boolean, level: Int): String =
    if (list.isEmpty) ""
    else print(list.head)(doPrint, level - (if (doPrint) 0 else 1)) + list.tail.map(print(_)(doPrint, level - (if (doPrint) 0 else 1))).foldLeft("")(_ + splitter.print(level) + _)

  def decapitalize(str: String): String =
    str.charAt(0).toLower.toString + str.substring(1)
}

//TODO remove term from the subroutine call