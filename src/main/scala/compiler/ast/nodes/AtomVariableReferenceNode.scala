package compiler.ast.nodes

class AtomVariableReferenceNode(parserText : String) extends AtomNode(parserText) {

  def this(){
    this("")
  }

  def setDereference() = {
    dRef = true
  }

  def getName(): String = {
    val id = find((c) => {
      c match {
        case c: IdentifierNode => true
        case _ => false
      }
    })

    id match{
      case Some(id) => id.getText
      case None => throw new Exception("Malformed VariableReferenceNode!")
    }
  }

  def isDereference(): Boolean = dRef


  var dRef: Boolean = false
}
