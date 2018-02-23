package compiler.ast.nodes

import compiler.ast.NodeBase
import compiler.ast.ASTType

class FunctionDeclarationNode(parserText : String) extends NodeBase(parserText) {

  def this(){
    this("")
  }

  def getParameterNode(): Option[NodeBase] ={
    find ((c) => {
      c match{
        case c: FormalParamsNode => true
        case _ => false
      }
    })
  }

  override def getType(): ASTType.Type = {
    val cType = find ((c) => {
      c match{
        case c: CompTypeNode => true
        case _ => false
      }
    })

    cType match {
      case Some(c) => c.getType()
      case None => ASTType.BAD
    }
  }

  def getName(): String = {
    val id = find((c) =>{
      c match{
        case c: IdentifierNode => true
        case _ => false
      }
    })

    id match {
      case Some(id) => id.getText
      case None => throw new Exception ("Malformed AST! function declaration has no id!")
    }
  }
}
