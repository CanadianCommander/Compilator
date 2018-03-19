package compiler.ir.nodes

import compiler.ir._


class IRReturnInstruction(retTmp: Option[IRTemporaryInstruction]) extends IRInstructionBase {

  override def toString(): String ={
    returnExpression match {
      case Some(r) => s"RETURN ${r.getString()};\n"
      case None => "RETURN;\n"
    }
  }

  private var returnExpression: Option[IRTemporaryInstruction] = retTmp
}
