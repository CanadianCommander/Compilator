package compiler.ir.nodes

import compiler.ir._


class IRFunctionInstruction(name: String,aLst: List[IRType.Type],ret: IRType.Type) extends IRInstructionBase {

  override def toString(): String ={
    s"FUNC ${myName} (${IRType.typeToString(argLst)}) ${IRType.typeToString(retTyp)}\n"
  }

  def getName(): String = name
  def getArgs(): List[IRType.Type] = aLst
  def getReturnT(): IRType.Type = ret 

  private var myName: String = name
  private var argLst: List[IRType.Type] = aLst
  private var retTyp: IRType.Type =ret
}
