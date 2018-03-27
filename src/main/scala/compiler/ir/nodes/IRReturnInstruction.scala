package compiler.ir.nodes

import compiler.ir._


class IRReturnInstruction(retTmp: Option[IRTemporaryInstruction]) extends IRInstructionBase {

  override def toString(): String ={
    returnExpression match {
      case Some(r) => s"RETURN ${r.getString()};\n"
      case None => "RETURN;\n"
    }
  }

  def getTemporary(): Option[IRTemporaryInstruction] = returnExpression

  private var returnExpression: Option[IRTemporaryInstruction] = retTmp
}
