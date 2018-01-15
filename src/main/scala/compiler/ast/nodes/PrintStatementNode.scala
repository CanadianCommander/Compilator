package compiler.ast.nodes

class PrintStatementNode(parserText : String) extends StatementNode(parserText) {

  def this(){
    this("")
  }
}
