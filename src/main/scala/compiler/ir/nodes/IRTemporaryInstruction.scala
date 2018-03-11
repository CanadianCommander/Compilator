package compiler.ir.nodes

import compiler.ir.IRInstructionBase
import compiler.ir.IRType

class IRTemporaryInstruction(id: Int, typ: IRType.Type, vName: String = "") extends IRInstructionBase {

  def getId() = tmpId
  def getVarName() = varName
  def getType() = myTyp

  override def toString(): String = {
    s"TEMP ${tmpId}:${IRType.typeToString(myTyp)};\n"
  }

  protected val myTyp: IRType.Type = typ
  protected val tmpId: Int = id
  protected val varName: String = vName
}
