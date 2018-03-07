package compiler.ir


import util.SimpleFactory
import util.logging.Logger._
import compiler.ast.NodeBase

class DefaultIRFactory extends SimpleFactory[Option[NodeBase], Option[IRNodeBase]] {

  override def create(root : Option[NodeBase]): Option[IRNodeBase] = {
    Some(new IRNodeBase())
  }
}
