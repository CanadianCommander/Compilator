package compiler.ir.nodes

import compiler.ir.IRNodeBase
import compiler.ir.IRType

class IRExpressionNode(left: Option[IRNodeBase], right: Option[IRNodeBase], op: Option[IROperatorNode] ) extends IRNodeBase {

  override def getType(): IRType.Type = {
    operator match {
      case Some(op) => {
        op.getType() match {
          case IRType.ARG => {
            leftHand.get.getType()
          }
          case IRType.Z => IRType.Z
          case _ => IRType.BAD
        }
      }
      case None => {
        //should only have left hand
        leftHand.get.getType()
      }
    }
  }

  override def toString(): String ={
    "" + left.getOrElse("") + op.getOrElse("") + right.getOrElse("") + buildMyString()
  }

  private def buildMyString():String = {
    if((!leftHand.isEmpty) && (!rightHand.isEmpty) && (!operator.isEmpty)){
      //binary op
      s"T${getTemporary().get.getId()} := T${leftHand.get.getTemporary().get.getId()} ${operator.get.getOperator()} T${rightHand.get.getTemporary().get.getId()};\n"
    }
    else {
      //unary
      s"T${getTemporary().get.getId()} := T${leftHand.get.getTemporary().get.getId()};\n"
    }
  }

  var leftHand = left
  var rightHand = right
  var operator = op
}
