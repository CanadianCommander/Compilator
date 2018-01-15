package compiler.ast.nodes

class OperationLessNode(parserText : String) extends OperationNode(parserText) {

  def this(){
    this("")
  }

  override def getOperator(): String = "<"
}
