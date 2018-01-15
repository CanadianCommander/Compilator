package compiler.ast.nodes

import compiler.ast.NodeBase

class StatementNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }
}
