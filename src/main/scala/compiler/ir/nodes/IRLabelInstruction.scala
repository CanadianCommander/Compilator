package compiler.ir.nodes

import compiler.ir._


class IRLabelInstruction(labelId: Int) extends IRInstructionBase {

  override def toString(): String ={
    s"L${id}:;\n"
  }

  def getId(): Int = id


  private val id = labelId
}
