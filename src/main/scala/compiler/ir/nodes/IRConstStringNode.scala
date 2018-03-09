package compiler.ir.nodes

import compiler.ir.IRType

class IRConstStringNode(s: String) extends IRConstNode[String] {

  override def getValue(): String ={
    myStr
  }

  override def getType(): IRType.Type = IRType.U

  private val myStr = s
}
