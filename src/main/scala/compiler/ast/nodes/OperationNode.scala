package compiler.ast.nodes

import compiler.ast.NodeBase

abstract class OperationNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }

  def getOperator(): String
}
