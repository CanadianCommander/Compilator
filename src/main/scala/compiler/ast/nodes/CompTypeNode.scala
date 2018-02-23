package compiler.ast.nodes

import compiler.ast.NodeBase

class CompTypeNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }
}
