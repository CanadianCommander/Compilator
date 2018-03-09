package compiler.ir.nodes

import compiler.ir.IRNodeBase
import compiler.ir.IRType

class IRTemporaryNode(id: Int, typ: IRType.Type, vName: String = "") extends IRNodeBase {

  def getId() = tmpId
  def getVarName() = varName

  override def getTemporary(): Option[IRTemporaryNode] = {
    Some(this)
  }

  override def toString(): String = {
    s"TEMP ${tmpId}:${IRType.typeToString(getType())};\n"
  }

  setType(typ)
  protected val tmpId: Int = id
  protected val varName: String = vName
}
