package compiler.ir

class IRNodeBase extends Traversable[IRNodeBase]{

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

  protected var children : Option[List[IRNodeBase]] = None
  protected var parent   : Option[IRNodeBase] = None
}
