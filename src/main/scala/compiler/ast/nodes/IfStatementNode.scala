package compiler.ast.nodes

class IfStatementNode(parserText : String) extends ConditionalStatementNode(parserText) {

  def this(){
    this("")
  }

  override def getKeyword(): String = {
    "if"
  }
}
