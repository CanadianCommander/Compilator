package compiler.ast.nodes

class ElseStatementNode(parserText : String) extends ConditionalStatementNode(parserText) {

  def this(){
    this("")
  }

  override def getKeyword(): String = {
    "else"
  }
}
