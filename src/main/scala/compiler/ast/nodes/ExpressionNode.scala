package compiler.ast.nodes

import compiler.ast.NodeBase

class ExpressionNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }

  //deal with case where by an expression holds only a reference to another expression
  //ex: expr(expr(...)). this is needless nesting and can be removed!!!!
  def removeRedudentChildren(){
    if(this.size == 1){
      val child = getChild(0)
      child match {
        case c: ExpressionNode =>{
          c.removeRedudentChildren()
          merge(c)
          removeChild(c)
        }
        case _ =>{
          //nop
        }
      }
    }
  }

  //merge this ExpressionNode with other
  def merge(other: ExpressionNode) = {
    setText(other.getText)
    other.foreach((otherChild)=>{
      this.addChild(otherChild)
    })
  }
}
