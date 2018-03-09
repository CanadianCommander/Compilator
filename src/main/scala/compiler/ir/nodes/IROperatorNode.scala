package compiler.ir.nodes

import compiler.ir.IRNodeBase
import compiler.ir.IRType
import compiler.ast.nodes.OperationNode

class IROperatorNode(op: OperationNode) extends IRNodeBase {

  override def getType(): IRType.Type = {
    myOp match{
      case "*" | "+" | "/" | "-"  => IRType.ARG
      case "<" | "==" => IRType.Z
    }
  }

  def getOperator():String = myOp

  private var myOp: String = op.getOperator()
}
