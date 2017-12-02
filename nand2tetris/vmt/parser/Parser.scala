package nand2tetris.vmt.parser

import nand2tetris.vmt.parser.Stack._

object  Parser {
  def parse(line: String): StackLine = line.replaceAll("//.+", "").trim.split(" ") match {
    case Array("") => Fluff
    case Array("pop", segment, i) => Memory(push = false, segment, i.toInt)
    case Array("push", segment, i) => Memory(push = true, segment, i.toInt)
    case Array("label", label) => Label(label)
    case Array("goto", label) => Jump(label, condition = false)
    case Array("if-goto", label) => Jump(label, condition = true)
    case Array("function", functionName, nArgs) => Function(functionName, nArgs.toInt)
    case Array("call", functionName, nArgs) => FunctionCall(functionName, nArgs.toInt)
    case Array("return") => Return
    case Array(op) => Op(op)
    case _ => Fluff
  }
}