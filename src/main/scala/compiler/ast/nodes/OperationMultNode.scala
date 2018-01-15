package compiler.ast.nodes

class OperationMultNode(parserText : String) extends OperationNode(parserText) {

  def this(){
    this("")
  }

  override def getOperator(): String = "*"
}
