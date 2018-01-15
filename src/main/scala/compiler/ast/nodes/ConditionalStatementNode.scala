package compiler.ast.nodes

import java.lang.UnsupportedOperationException

class ConditionalStatementNode(parserText : String) extends StatementNode(parserText) {

  def this(){
    this("")
  }

  def getKeyword(): String = {
    throw new UnsupportedOperationException("getKeyword not implemented")
  }
}
