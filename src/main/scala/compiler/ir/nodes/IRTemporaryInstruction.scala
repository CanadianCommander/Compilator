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

  def getString(): String ={
    myDerefExp match {
      case Some(de) => {
        s"T${getId()}[${de.getString()}]"
      }
      case None => {
        s"T${getId()}"
      }
    }
  }

  def setDereferenceTemp(t: IRTemporaryInstruction) ={
    myDerefExp = Some(t)
  }

  def getCopy(): IRTemporaryInstruction = {
    val nTmp = new IRTemporaryInstruction(tmpId,myTyp,varName)
    if(myDerefExp != None){
      nTmp.setDereferenceTemp(myDerefExp.get)
    }
    nTmp
  }

  //remove array type
  def stripType() = {
    myTyp = IRType.removeArrayType(myTyp)
  }

  protected var myDerefExp: Option[IRTemporaryInstruction] = None
  protected var myTyp: IRType.Type = typ
  protected val tmpId: Int = id
  protected val varName: String = vName
}
