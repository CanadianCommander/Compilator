package compiler.ir.nodes

import compiler.ir.IRNodeBase
import compiler.ast.nodes.AtomLiteralNode

abstract class IRConstNode[T] extends IRNodeBase {
  def getValue(): T

  override def toString(): String ={
    s"T${getTemporary().get.getId} := ${getValue()};\n"
  }
}

object EzMk {
  def IRConstNode (atm: AtomLiteralNode): IRConstNode[_] = {
    try{
      new IRConstIntegerNode(atm.getText.toInt)
    }
    catch {
      case e: Exception => {
        new IRConstStringNode("FOOOOO!")
      }
    }

  }
}
