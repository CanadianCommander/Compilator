package compiler.ast.nodes

import java.lang.UnsupportedOperationException

class ConditionalStatementNode(parserText : String) extends StatementNode(parserText) {

  def this(){
    this("")
  }

  def getConditionExp(): ExpressionNode = {
    val exp = find ((c) => {
      c match{
        case c: ExpressionNode => true
        case _ => false
      }
    })

    exp match{
      case Some(e) => e.asInstanceOf[ExpressionNode]
      case None => throw new Exception("Malformed AST. ConditionalStatement does not have a condition expression!")
    }
  }

  def getKeyword(): String = {
    throw new UnsupportedOperationException("getKeyword not implemented")
  }
}
