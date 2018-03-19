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

  def getExpression(): ExpressionNode = {
    if(isDereference()){
      val exp = find((c) => {
        c match {
          case c: ExpressionNode => true
          case _ => false
        }
      })

      exp.getOrElse(throw new Exception("var ref that is dereference has no expression")).asInstanceOf[ExpressionNode]
    }
    else{
      throw new Exception ("Var ref is not a dereference!")
    }
  }

  def isDereference(): Boolean = dRef


  var dRef: Boolean = false
}
