package compiler.ir.nodes

import compiler.ir.IRType

class IRConstFloatNode(i: Float) extends IRConstNode[Float] {

  override def getValue(): Float = {
    myNum
  }

  override def getType(): IRType.Type = IRType.F

  private var myNum = i
}
