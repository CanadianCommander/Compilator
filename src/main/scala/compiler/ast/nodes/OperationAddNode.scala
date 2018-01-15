package compiler.ast.nodes

class OperationAddNode(parserText : String) extends OperationNode(parserText) {

  def this(){
    this("")
  }

  override def getOperator(): String = "+"
}
