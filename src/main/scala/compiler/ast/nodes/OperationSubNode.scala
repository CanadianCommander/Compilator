package compiler.ast.nodes

class OperationSubNode(parserText : String) extends OperationNode(parserText) {

  def this(){
    this("")
  }

  override def getOperator(): String = "-"
}
