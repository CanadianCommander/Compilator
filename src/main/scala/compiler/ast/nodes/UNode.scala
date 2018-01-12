package compiler.ast.nodes

import compiler.ast.NodeBase

//UnknownNode (sort name for convenience )
class UNode(parserText: String) extends NodeBase(parserText) {
  //nothing
  def this(){
    this("")
  }
}
