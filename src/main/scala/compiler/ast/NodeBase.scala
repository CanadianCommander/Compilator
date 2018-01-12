package compiler.ast

import scala.collection.Traversable

abstract class NodeBase(parserText: String) extends Traversable[NodeBase]{

  def this(){
    this("")
  }

  def addChild(child: NodeBase){
    children = _lazyListAllocation(children, child)
    child.setParent(this)
  }

  def getText: String = {
    parent match {
      case Some(p)  => p.getChildTextPrefix() + _getTextBracket(nodeText) + p.getChildTextSufix()
      case None     => _getTextBracket(nodeText)
    }
  }

  //return the string with which all children should be prefixed
  def getChildTextPrefix(): String = {""}

  //return the string with which all children should be sufixed
  def getChildTextSufix(): String = {""}

  def setText(text: String) = {
    nodeText = text
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

  //does node have nice text output (returned from getText)
  def isNiceText(): Boolean = {
    false
  }

  //iterate over child nodes
  override def foreach[U](f: NodeBase => U){
    children match{
      case Some(list) => list.foreach(f)
      case None       => None
    }
  }

  protected def _getTextBracket(text: String): String = {
    s"<${text}>"
  }

  protected def _lazyListAllocation(list: Option[List[NodeBase]], childToAdd: NodeBase): Option[List[NodeBase]] = {
    list match {
      case Some(l) => Some(l :+ childToAdd)
      case None    => Some(List(childToAdd))
    }
  }

  //concat all text returned by every child's getText method in order
  protected def _concatChildText(): String = {
    children match {
      case Some(l)   => l.map((x) => x.getText).reduceLeft((acc,x) => acc + x)
      case None      => ""
    }
  }

  protected var nodeText : String = parserText
  protected var parent   : Option[NodeBase] = None
  protected var children : Option[List[NodeBase]] = None
}
