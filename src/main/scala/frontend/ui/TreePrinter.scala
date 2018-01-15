package frontend.ui

import java.io.PrintStream

import compiler.ast.NodeBase
import compiler.ast.nodes._
import util.logging.Logger._

//prints out a nicely formated tree to the given input stream
class TreePrinter(treeRoot: NodeBase, outStream: PrintStream = System.out) {


  def printTree() = {
    //out.print(root.getText)
    DFS(root)
  }

  def setOutputStream(outStream: PrintStream) = {
    out = outStream
  }

  protected def DFS(node: NodeBase): Unit = {
    //print all children surounded by some inclosing text
    val encloseChildren = (n: NodeBase) =>{
      out.print(getChildPrefix(n.getParent()))
      n.foreach((child) => DFS(child))
      out.print(getChildSufix(n.getParent()))
    }

    //print out node enclosed by parent prefix and suffix
    val printWithPreSufix = (n: NodeBase) => out.print(getChildPrefix(Some(n)) + n.getText + getChildSufix(Some(n)))

    node match {
      case n: FormalParamsNode =>{
        out.print("(")
        n.foreach((child) => DFS(child))
        if(!n.isLeaf()){
          out.print("\b\b) ")
        }
        else{
          out.print(")")
        }
      }
      case n: FunctionBodyNode =>{
        out.print("{\n")
        n.foreach((child) => DFS(child))
        out.print("}\n\n")
      }
      case n: FunctionDeclarationNode => {
        encloseChildren(n)
      }
      case n: FunctionArgNode => {
        encloseChildren(n)
      }
      case n: VariableDeclarationNode =>{
        out.print(getChildPrefix(n.getParent()))
        n.foreach((child) => DFS(child))
        out.print("\b;" + getChildSufix(n.getParent()))
      }
      case n: AssignStatementNode =>{
        out.print(s"${getChildPrefix(n.getParent())}")
        DFS(n.getChild(0))
        if(n.size > 1){
          out.print("\b=")
          DFS(n.getChild(1))
        }
        out.print(s"\b;${getChildSufix(n.getParent())}")
      }
      case n: PrintStatementNode => {
        out.print(s"${getChildPrefix(n.getParent())}${n.getText} ")
        n.foreach((c)=>DFS(c))
        out.print(s"\b;${getChildSufix(n.getParent())}")
      }
      case n: OperationNode =>{
        n.foreach((c) => {
          out.print(s"\b${n.getOperator()}")
          DFS(c)
        })
      }
      case n: AtomVariableReferenceNode =>{
        if(n.size == 2){
          //var ref is in format "id[<exp>]"
          DFS(n.getChild(0))
          out.print("\b[")
          DFS(n.getChild(1))
          out.print("\b] ")
        }
        else{
          n.foreach((c)=>DFS(c))
        }
      }
      case n: ReturnStatementNode => {
        out.print(s"${getChildPrefix(n.getParent())}return ")
        n.foreach((c) => DFS(c))
        out.print(s"\b;${getChildSufix(n.getParent())}")
      }
      case n: FunctionCallArgumentsNode => {
        out.print("\b(")
        n.foreach((c) => {
          DFS(c)
          out.print("\b, ")
        })
        if(!n.isLeaf()){
          out.print("\b\b) ")
        }
        else{
          out.print(") ")
        }
      }
      case n: ElseStatementNode        => {
        //special case of ConditionalStatementNode. MUST COME BEFORE!!!
        out.print(s"${getChildPrefix(n.getParent())}${n.getKeyword()}")
        out.print(s"${getChildSufix(n.getParent())}")
        n.foreach((c) => DFS(c))
      }
      case n: ConditionalStatementNode => {
        out.print(s"${getChildPrefix(n.getParent())}${n.getKeyword()}")
        if(n.size == 2){
          out.print(" (")
          DFS(n.getChild(0))
          out.print(s"\b) ${getChildSufix(n.getParent())}")
          DFS(n.getChild(1))
        }
        else{
          //should never happen
          logMsg("Bad Tree Format", Level.ERROR)
        }
      }
      case n: BlockNode  => {
        out.print(s"${getChildPrefix(n.getParent())}{\n")
        n.foreach((c) => DFS(c))
        out.print(s"${getChildPrefix(n.getParent())}}\n")
      }
      case n: NodeBase => {
        if(n.isLeaf()){
          printWithPreSufix(n)
        }
        n.foreach((child) => DFS(child))
      }
    }
  }

  protected def getChildPrefix(parent: Option[NodeBase]): String = {
    parent match {
      case Some(par) => {
        par match {
          case _: VariableDeclarationNode => ""
          case _: PrintStatementNode      => ""
          case _: ReturnStatementNode     => ""
          case _: AssignStatementNode     => ""
          case n: ExpressionNode          => {
            //block recursion if we are child to while / if statment
            var expParent = n.getParent()
            expParent match {
              case Some(nP) => {
                nP match {
                  case p: ConditionalStatementNode  => ""
                  case p: NodeBase                  => getChildPrefix(expParent)
                }
              }
              case None => ""
            }
          }
          case p: FunctionBodyNode        => getChildPrefix(p.getParent()) + "    "
          case p: BlockNode               => getChildPrefix(p.getParent()) + "    "
          case p: NodeBase                => getChildPrefix(p.getParent())
        }
      }
      case None => ""
    }
  }

  protected def getChildSufix(parent: Option[NodeBase]): String = {
    parent match {
      case Some(par) => {
        par match {
          case _: FunctionNode              => "\n"
          case _: FunctionBodyNode          => "\n"
          case _: BlockNode                 => "\n"
          case _: FunctionDeclarationNode   => " "
          case _: FormalParamsNode          => "\b, "
          case _: FunctionArgNode           => " "
          case _: VariableDeclarationNode   => " "
          case _: AssignStatementNode       => " "
          case _: PrintStatementNode        => " "
          case _: ReturnStatementNode       => " "
          case _: ConditionalStatementNode  => " "
          case _: FunctionCallArgumentsNode => " "
          case p: NodeBase                  => getChildSufix(p.getParent())
        }
      }
      case None => " "
    }
  }

  private var out: PrintStream = outStream
  private val root: NodeBase = treeRoot
}
