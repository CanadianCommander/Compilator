package compiler.ast.nodes

class AtomLiteralNode(parserText : String) extends AtomNode(parserText) {

  def this(){
    this("")
  }
}
