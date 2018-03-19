package compiler.ir.nodes

import compiler.ir._

class IRAssignInstruction(targ: IRTemporaryInstruction, exp: IRExpression) extends IRInstructionBase {


  override def toString(): String = {
    s"${target.getString()} := " + expression.toString() + ";\n"
  }

  private var expression: IRExpression = exp
  private var target: IRTemporaryInstruction = targ
}
