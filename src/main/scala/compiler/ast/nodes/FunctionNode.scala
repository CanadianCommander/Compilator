package compiler.ast.nodes

import compiler.ast.NodeBase

class FunctionNode(parserText : String) extends NodeBase(parserText) {

  def this(fDec: FunctionDeclarationNode, fBody: FunctionBodyNode){
    this("")
    addChild(fDec)
    addChild(fBody)

    dec = fDec
    body = fBody
  }

  def getFunctionDeclaration(): FunctionDeclarationNode = dec
  def getFunctionBody(): FunctionBodyNode = body


  private var dec: FunctionDeclarationNode = null
  private var body: FunctionBodyNode = null
}
