package compiler.ast.nodes

import compiler.ast.NodeBase

class FunctionCallArgumentsNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }
}
