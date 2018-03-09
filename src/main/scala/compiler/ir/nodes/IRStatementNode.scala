package compiler.ir.nodes

import compiler.ir._
import compiler.ast.nodes.AssignStatementNode

class IRStatementNode(targ: Option[IRNodeBase] , exp: Option[IRNodeBase]) extends IRNodeBase {

  def this(){
    this(None,None)
  }

  def setExpression(nE: IRNodeBase) = expression = Some(nE)
  def setTarget(nT: IRNodeBase) = target = Some(nT)

  override def getTemporary(): Option[IRTemporaryNode] = {
    targ match{
      case Some(t: IRTemporaryNode) => Some(t)
      case _ => throw new IRException("statment target is not temporary!")
    }
  }

  override def toString(): String = {
    var outS = ""
    if(!expression.isEmpty) outS = outS + expression.get.toString()
    if(!target.isEmpty){
      val targTmp = target.get.getTemporary()
      var expTmp = expression.get.getTemporary()
      if(!targTmp.isEmpty && !expTmp.isEmpty){
        outS = outS + s"T${targTmp.get.getId()} := T${expTmp.get.getId()};\n"
      }
      else {
        outS = outS + target.get.toString()
      }
     }
    outS
  }

  protected var expression: Option[IRNodeBase] = exp
  protected var target: Option[IRNodeBase] = targ
}
