package nand2tetris.compiler.grammar

object LexicalElements {
  trait LexicalElement {
    def print(level: Int = 0): String = (" " * level * 2) + (this match {
      case Keyword(str) => s"<keyword> $str </keyword>"
      case Symbol(s) => s"<symbol> $s </symbol>"
      case ConstantInteger(int) => s"<integerConstant> $int </integerConstant>"
      case ConstantString(str) => s"<stringConstant> $str </stringConstant>"
      case Identifier(i) => s"<identifier> $i </identifier>"
      case _ => ""
    }) + "\n"
  }

  case class Keyword(str: String) extends LexicalElement
  case class Symbol(char: Char) extends LexicalElement
  case class ConstantInteger(int: Int) extends LexicalElement
  case class ConstantString(str: String) extends LexicalElement
  case class Identifier(str: String) extends LexicalElement
  case class Unknown(str: String) extends LexicalElement
  case class Error(str: String) extends LexicalElement

  final val CLASS = Keyword("class")
  final val CONSTRUCTOR = Keyword("constructor")
  final val FUNCTION = Keyword("function")
  final val METHOD = Keyword("method")
  final val FIELD = Keyword("field")
  final val STATIC = Keyword("static")
  final val VAR = Keyword("var")
  final val INT = Keyword("int")
  final val CHAR = Keyword("char")
  final val BOOLEAN = Keyword("boolean")
  final val VOID = Keyword("void")
  final val TRUE = Keyword("true")
  final val FALSE = Keyword("false")
  final val NULL = Keyword("null")
  final val THIS = Keyword("this")
  final val LET = Keyword("let")
  final val DO = Keyword("do")
  final val IF = Keyword("if")
  final val ELSE = Keyword("else")
  final val WHILE = Keyword("while")
  final val RETURN = Keyword("return")

  final val BRACE_O = Symbol('{')
  final val BRACE_C = Symbol('}')
  final val PARENTHESIS_O = Symbol('(')
  final val PARENTHESIS_C = Symbol(')')
  final val BRACKET_SQ_O = Symbol('[')
  final val BRACKET_SQ_C = Symbol(']')
  final val BRACKET_AG_O = Symbol('<')
  final val BRACKET_AG_C = Symbol('>')
  final val POINT = Symbol('.')
  final val COMMA = Symbol(',')
  final val SEMICOLON = Symbol(';')
  final val PLUS = Symbol('+')
  final val MINUS = Symbol('-')
  final val ASTERISK = Symbol('*')
  final val SLASH = Symbol('/')
  final val AND = Symbol('&')
  final val OR = Symbol('|')
  final val NOT = Symbol('~')
  final val EQUALS = Symbol('=')

  private final val SYMBOLS = "{}()[]<>.,;+-*/&|~="

  def keywordFromString(str: String): Option[Keyword] = str match {
    case "class" => Some(CLASS)
    case "constructor" => Some(CONSTRUCTOR)
    case "function" => Some(FUNCTION)
    case "method" => Some(METHOD)
    case "field" => Some(FIELD)
    case "static" => Some(STATIC)
    case "var" => Some(VAR)
    case "int" => Some(INT)
    case "char" => Some(CHAR)
    case "boolean" => Some(BOOLEAN)
    case "void" => Some(VOID)
    case "true" => Some(TRUE)
    case "false" => Some(FALSE)
    case "null" => Some(NULL)
    case "this" => Some(THIS)
    case "let" => Some(LET)
    case "do" => Some(DO)
    case "if" => Some(IF)
    case "else" => Some(ELSE)
    case "while" => Some(WHILE)
    case "return" => Some(RETURN)
    case _ => None
  }

  def symbolFromChar(char: Char): Option[Symbol] =
    if(isSymbol(char)) Some(Symbol(char))
    else None

  def isSymbol(char: Char): Boolean =
    SYMBOLS.contains(char)

  def isIdentifier(s: String) =
    s.matches("^[a-zA-Z_]\\w*$")
}