package compiler.ast.nodes

import compiler.ast.NodeBase

class FunctionBodyNode(parserText : String) extends NodeBase(parserText + "\n") {

  def this(){
    this("")
  }

  override def setText(text: String) = {
    nodeText = text + "\n"
  }

  override def getChildTextPrefix(): String = {"    "}
}
