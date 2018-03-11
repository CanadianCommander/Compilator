package compiler.ir.nodes

import compiler.ir._

class IRExpression(lft: Option[IRTemporaryInstruction], op: Option[IROperator], rht: Option[IRTemporaryInstruction]) extends IRInstructionBase {

  def this(op: Option[IROperator], rht: Option[IRTemporaryInstruction]) = {
    this(None,op,rht)
  }

  override def toString():String = {
    if(!(left.isEmpty) && !(right.isEmpty)){
      s"T${left.get.getId()} ${operator.get.toString()} T${right.get.getId()}"
    }
    else {
      s"${operator.get.toString()} T${right.get.getId()}"
    }
  }

  private var left: Option[IRTemporaryInstruction] = lft
  private var right: Option[IRTemporaryInstruction] = rht
  private var operator: Option[IROperator] = op
}
