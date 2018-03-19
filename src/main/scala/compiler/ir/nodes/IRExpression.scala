package compiler.ir.nodes

import compiler.ir._

class IRExpression(lft: Option[IRTemporaryInstruction], op: Option[IROperator], rht: Option[IRTemporaryInstruction]) extends IRInstructionBase {

  def this(op: Option[IROperator], rht: Option[IRTemporaryInstruction]) = {
    this(None,op,rht)
  }

  override def toString():String = {
    if(!(left.isEmpty) && !(right.isEmpty)){
      s"${left.get.getString()} ${operator.get.toString()} ${right.get.getString()}"
    }
    else {
      s"${operator.get.toString()} ${right.get.getString()}"
    }
  }

  private var left: Option[IRTemporaryInstruction] = lft
  private var right: Option[IRTemporaryInstruction] = rht
  private var operator: Option[IROperator] = op
}
