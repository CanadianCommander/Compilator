package compiler.ast.nodes

import compiler.ast.NodeBase

class FunctionBodyNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }
}
