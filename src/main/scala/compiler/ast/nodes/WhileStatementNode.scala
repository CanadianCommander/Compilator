package compiler.ast.nodes

class WhileStatementNode(parserText : String) extends ConditionalStatementNode(parserText) {

  def this(){
    this("")
  }

  override def getKeyword(): String = {
    "while"
  }
}
