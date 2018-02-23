package compiler.ast.nodes

import compiler.ast.NodeBase
import compiler.ast.ASTType

class VariableDeclarationNode(parserText : String) extends NodeBase(parserText) {

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
      case None => throw new Exception("Malformed VariableReferenceNode!")
    }
  }

  override def getType(): ASTType.Type = {
    val myTypeNode = find ((c) => {
      c match{
        case c: CompTypeNode => true
        case _ => false
      }
    })

    myTypeNode match{
      case Some(mtn) => mtn.getType()
      case None => throw new Exception ("Varialbe Declaration has not CompTypeNode!")
    }
  }
}
