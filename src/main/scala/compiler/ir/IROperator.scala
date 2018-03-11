package compiler.ir

class IROperator(operator: IROperator.Type,typ: IRType.Type) {

  override def toString(): String = {
    if(myOp != IROperator.NOP){
      s"${IROperator.operatorToString(myOp)} ${IRType.typeToString(myType)}"
    }
    else {
      ""
    }
  }

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

  type Type = Value
  val ADD, SUB, MUL, DIV, LESS, EQ, NOT, NOP = Value
}
