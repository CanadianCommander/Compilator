package compiler.ir

import compiler.ir.nodes._

class IRNodeBase extends Traversable[IRNodeBase]{

  def setType(nTyp: IRType.Type) = {
    resType = nTyp
  }

  def assignTemporary(tmp: IRTemporaryNode) = {
    myTemporary = Some(tmp)
  }

  def getTemporary() : Option[IRTemporaryNode] = {
    myTemporary
  }

  def getType(): IRType.Type ={
    resType
  }

  def getChild(index: Int): IRNodeBase = {
    children match {
      case Some(l) => l(index)
      case None    => throw new IndexOutOfBoundsException(s"$index is out of bounds")
    }
  }

  override def size: Int = {
    children match{
      case Some(l) => l.size
      case None    => 0
    }
  }

  def addChild(child: IRNodeBase){
    if(child != null){
      children = _lazyListAllocation(children, child)
      child.setParent(this)
    }
  }

  def addChild(child: List[IRNodeBase]){
    child.foreach((c) => addChild(c))
  }

  def removeChild(child: IRNodeBase){
    children match {
      case Some(cl) => children = Option(cl.filter((c) => c != child))
      case None => //nop
    }
  }

  def setParent(newParent: IRNodeBase){
    parent = Some(newParent)
  }

  def isLeaf(): Boolean = {
    children match {
      case Some(list) => (list.size == 0)
      case None    => true
    }
  }

  //iterate over child nodes
  override def foreach[U](f: IRNodeBase => U){
    children match{
      case Some(list) => list.foreach(f)
      case None       => None
    }
  }

  protected def _lazyListAllocation(list: Option[List[IRNodeBase]], childToAdd: IRNodeBase): Option[List[IRNodeBase]] = {
    list match {
      case Some(l) => Some(l :+ childToAdd)
      case None    => Some(List(childToAdd))
    }
  }

  override def toString(): String = {
    foldLeft("")((acc,c) => acc + c)
  }

  protected var children : Option[List[IRNodeBase]] = None
  protected var parent   : Option[IRNodeBase] = None
  protected var resType   : IRType.Type = IRType.BAD
  protected var myTemporary: Option[IRTemporaryNode] = None
}
