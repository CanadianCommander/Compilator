package compiler.ast.nodes

class AtomFunctionCallNode(parserText : String) extends AtomNode(parserText) {

  def this(){
    this("")
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
      case None => throw new Exception("Malformed AST, Function call has no identifier!")
    }
  }

  def getArgList(): List[ExpressionNode] = {
    val argsNode = find ((c) => {
      c match{
        case c: FunctionCallArgumentsNode => true
        case _ => false
      }
    })

    argsNode match{
      case Some(args) => args.toList.asInstanceOf[List[ExpressionNode]]
      case None => List()
    }
  }
}
