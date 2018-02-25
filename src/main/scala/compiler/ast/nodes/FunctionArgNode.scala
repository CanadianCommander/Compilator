package compiler.ast.nodes

import compiler.ast.NodeBase
import compiler.ast.ASTType

class FunctionArgNode(parserText : String) extends NodeBase(parserText) {

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
      case None => throw new Exception("Malformed FunctionArgNode!")
    }
  }

  override def getType(): ASTType.Type = {
    val cType = find ((c) => {
      c match{
        case c: CompTypeNode => true
        case _ => false
      }
    })
    cType match{
      case Some(ct) => ct.getType()
      case None => throw new Exception("Malformed AST! Function arg does not have type!")
    }
  }

  def getTypeNode(): CompTypeNode = {
    val cType = find ((c) => {
      c match{
        case c: CompTypeNode => true
        case _ => false
      }
    })
    cType match{
      case Some(ct) => ct.asInstanceOf[CompTypeNode]
      case None => throw new Exception("Malformed AST! Function arg does not have type!")
    }
  }
}
