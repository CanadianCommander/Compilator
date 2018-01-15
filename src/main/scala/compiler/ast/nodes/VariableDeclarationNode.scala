package compiler.ast.nodes

import compiler.ast.NodeBase

class VariableDeclarationNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }
}
