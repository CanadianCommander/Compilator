package compiler.ir.nodes

import compiler.ir._

class IRConst[T](typ: IRType.Type, value: T) extends IRInstructionBase {

  def getType(): IRType.Type = myType
  def getValue(): T = myValue

  override def toString(): String ={
    if(myType == IRType.Z){
      myValue.toString().capitalize
    }
    else{
      myValue.toString()
    }
  }

  private val myType = typ
  private val myValue = value
}
