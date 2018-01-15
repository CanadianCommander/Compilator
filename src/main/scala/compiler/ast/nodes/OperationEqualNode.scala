package compiler.ast.nodes

class OperationEqualNode(parserText : String) extends OperationNode(parserText) {

  def this(){
    this("")
  }

  override def getOperator(): String = "=="
}
