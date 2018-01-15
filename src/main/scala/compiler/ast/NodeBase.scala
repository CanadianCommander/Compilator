package compiler.ast

import scala.collection.Traversable
import java.lang.IndexOutOfBoundsException

abstract class NodeBase(parserText: String) extends Traversable[NodeBase]{

  def this(){
    this("")
  }

  def getText: String = {
    _getTextBracket(nodeText)
  }

  def getParent(): Option[NodeBase] = {
    parent
  }

  def getChild(index: Int): NodeBase = {
    children match {
      case Some(l) => l(index)
      case None    => throw new IndexOutOfBoundsException(s"$index is out of bounds")
    }
  }

  def setText(text: String) = {
    nodeText = text
  }

  def addChild(child: NodeBase){
    if(child != null){
      children = _lazyListAllocation(children, child)
      child.setParent(this)
    }
  }

  def removeChild(child: NodeBase){
    children match {
      case Some(cl) => children = Option(cl.filter((c) => c != child))
      case None => //nop
    }
  }

  def setParent(newParent: NodeBase){
    parent = Some(newParent)
  }

  def isLeaf(): Boolean = {
    children match {
      case Some(list) => (list.size == 0)
      case None    => true
    }
  }

  override def size: Int = {
    children match{
      case Some(l) => l.size
      case None    => 0
    }
  }

  //iterate over child nodes
  override def foreach[U](f: NodeBase => U){
    children match{
      case Some(list) => list.foreach(f)
      case None       => None
    }
  }

  private def _getTextBracket(text: String): String = {
    s"${text}"
  }

  protected def _lazyListAllocation(list: Option[List[NodeBase]], childToAdd: NodeBase): Option[List[NodeBase]] = {
    list match {
      case Some(l) => Some(l :+ childToAdd)
      case None    => Some(List(childToAdd))
    }
  }

  protected var nodeText : String = parserText
  protected var parent   : Option[NodeBase] = None
  protected var children : Option[List[NodeBase]] = None
}
