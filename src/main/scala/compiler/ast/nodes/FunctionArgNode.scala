package compiler.ast.nodes

import compiler.ast.NodeBase

class FunctionArgNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }
}
