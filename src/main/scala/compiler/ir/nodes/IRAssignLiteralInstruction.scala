package compiler.ir.nodes

import compiler.ir._

class IRAssignLiteralInstruction(targ: IRTemporaryInstruction,lit: IRConst[_]) extends IRInstructionBase {


  override def toString(): String = {
    s"${target.getString()} := ${literal.toString()};\n"
  }

  def getLiteral(): IRConst[_] = literal
  def getTemporary(): IRTemporaryInstruction = target

  private var literal: IRConst[_] = lit
  private var target: IRTemporaryInstruction = targ
}
