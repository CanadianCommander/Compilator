package compiler.ir.nodes

import compiler.ir._


class IRNewArrayInstruction(tmp: IRTemporaryInstruction, siz: Int) extends IRInstructionBase {

  override def toString(): String ={
    s"${targetTmp.getString()} := NEWARRAY${IRType.removeArrayType(targetTmp.getType())} ${arraySize};\n"
  }

  private val arraySize = siz
  private val targetTmp = tmp
}
