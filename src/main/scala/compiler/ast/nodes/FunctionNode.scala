package compiler.ast.nodes

import compiler.ast.NodeBase

class FunctionNode(parserText : String) extends NodeBase(parserText + "\n") {

  def this(){
    this("")
  }

  override def getChildTextPrefix():String = {"\n"}
  override def getChildTextSufix():String = {"\n"}
}
