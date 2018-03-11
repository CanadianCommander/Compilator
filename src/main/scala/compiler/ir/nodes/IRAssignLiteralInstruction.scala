package compiler.ir.nodes

import compiler.ir._

class IRAssignLiteralInstruction(targ: IRTemporaryInstruction,lit: IRConst[_]) extends IRInstructionBase {


  override def toString(): String = {
    s"T${target.getId()} := ${literal.toString()};\n"
  }

  private var literal: IRConst[_] = lit
  private var target: IRTemporaryInstruction = targ
}
