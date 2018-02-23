package compiler.ast.nodes

import compiler.ast.NodeBase
import compiler.ast.ASTType

abstract class OperationNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }

  def getValidTypeFilter(): (ASTType.Type) => Boolean = {
    (t: ASTType.Type) => true
  }

  def getOperator(): String
}
