package compiler.ast.nodes

import compiler.ast.NodeBase

class CompTypeNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }

  def setLen(l: Int) ={
    myLen = l
  }

  def setLenS(s: String) = {
    myLen = s.toInt
  }

  override def getLen(): Int = {myLen}

  private var myLen: Int = 0
}
