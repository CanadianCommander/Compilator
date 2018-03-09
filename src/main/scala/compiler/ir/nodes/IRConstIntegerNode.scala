package compiler.ir.nodes

import compiler.ir.IRType

class IRConstIntegerNode(i: Int) extends IRConstNode[Int] {

  override def getValue(): Int = {
    myNum
  }

  override def getType(): IRType.Type = IRType.I

  private var myNum = i
}
