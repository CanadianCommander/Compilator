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

  def getBlock(): BlockNode = {
    val block = find((c) => {
      c match {
        case c: BlockNode => true
        case _ => false
      }
    })

    if(block != None){
      block.get.asInstanceOf[BlockNode]
    }
    else {
      throw new Exception("Malformed AST. Conditional Statement has no block!")
    }
  }

  def getKeyword(): String = {
    throw new UnsupportedOperationException("getKeyword not implemented")
  }
}
