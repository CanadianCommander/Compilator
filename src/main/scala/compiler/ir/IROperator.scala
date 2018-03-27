package compiler.ir

import compiler.ast.nodes.OperationNode;

class IROperator(operator: IROperator.Type,typ: IRType.Type) {

  override def toString(): String = {
    if(myOp != IROperator.NOP){
      s"${IRType.typeToString(myType)} ${IROperator.operatorToString(myOp)}"
    }
    else {
      ""
    }
  }

  def getType(): IRType.Type = myType
  def getOperation(): IROperator.Type = myOp

  private var myType: IRType.Type = typ
  private var myOp: IROperator.Type = operator
}

object IROperator extends Enumeration{

  def newNop(): IROperator = {
    new IROperator(IROperator.NOP,IRType.BAD)
  }

  def operatorToString(op: Type): String ={
    op match{
      case ADD => "+"
      case SUB => "-"
      case MUL => "*"
      case DIV => "/"
      case LESS => "<"
      case EQ  => "=="
      case NOT => "!"
      case NOP => ""
    }
  }

  def operatorFromString(str: String): Type = {
    str match{
      case "+" => ADD
      case "-" => SUB
      case "*" => MUL
      case "/" => DIV
      case "<" => LESS
      case "==" => EQ
      case "!" => NOT
      case _ => NOP
    }
  }

  def operatorFromAST(astOp: OperationNode, opTyp: IRType.Type): IROperator = {
    val operand = operatorFromString(astOp.getOperator())
    new IROperator(operand, opTyp)
  }

  type Type = Value
  val ADD, SUB, MUL, DIV, LESS, EQ, NOT, NOP = Value
}
