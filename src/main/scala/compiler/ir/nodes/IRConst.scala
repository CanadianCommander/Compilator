package compiler.ir.nodes

import compiler.ir._

class IRConst[T](typ: IRType.Type, value: T) extends IRInstructionBase {

  def getType(): IRType.Type = myType
  def getValue(): T = myValue

  override def toString(): String ={
    myValue.toString()
  }

  private val myType = typ
  private val myValue = value
}
