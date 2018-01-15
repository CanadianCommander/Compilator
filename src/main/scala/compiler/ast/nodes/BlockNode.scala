package compiler.ast.nodes

import compiler.ast.NodeBase

class BlockNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }
}
