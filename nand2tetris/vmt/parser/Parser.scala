package nand2tetris.vmt.parser

import nand2tetris.vmt.parser.Stack._

object Parser {
  def parse(line: String): StackLine =
    if (line.length > 0 && !line.startsWith("\\\\")) {
      line.split(" ") match {
        case Array("pop", segment, i) => Memory(push = false, segment, i.toInt)
        case Array("push", segment, i) => Memory(push = true, segment, i.toInt)
        case Array(op) => Op(op)
        case _ => Fluff
      }
    } else Fluff
}