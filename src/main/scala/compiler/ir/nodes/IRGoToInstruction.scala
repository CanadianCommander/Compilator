package compiler.ir.nodes

import compiler.ir._


class IRGoToInstruction(condition: Option[IRTemporaryInstruction], jumpTarget: IRLabelInstruction) extends IRInstructionBase {

  override def toString(): String ={
    if(myCond != None){
      s"IF ${myCond.get.getString()} GOTO L${jumpTarget.getId()};\n"
    }
    else {
      s"GOTO L${jumpTarget.getId()};\n"
    }
  }


  private val myCond: Option[IRTemporaryInstruction] = condition
  private val jmp: IRLabelInstruction = jumpTarget
}
