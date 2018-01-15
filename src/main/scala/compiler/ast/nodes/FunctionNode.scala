package compiler.ast.nodes

import compiler.ast.NodeBase

class FunctionNode(parserText : String) extends NodeBase(parserText) {

  def this(fDec: FunctionDeclarationNode, fBody: FunctionBodyNode){
    this("")
    addChild(fDec)
    addChild(fBody)
  }
}
