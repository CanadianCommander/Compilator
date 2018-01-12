package frontend.ui

import java.io.PrintStream

import compiler.ast.NodeBase

//prints out a nicely formated tree to the given input stream
class TreePrinter(treeRoot: NodeBase, outStream: PrintStream = System.out) {


  def printTree() = {
    //out.print(root.getText)
    DFS(root)
  }

  //breadth first tree traversal
  def BFS(){
    var frontier = List(root)

    while(frontier.size > 0){
      var newFrontier = List[NodeBase]()
      frontier.foreach((node)=>{
        if(node.isLeaf()){
          out.print(node.getText)
        }

        node.foreach((child) => newFrontier = newFrontier :+ child)
      })
      frontier = newFrontier
    }
  }

  def DFS(node: NodeBase){
    if(node.isLeaf() || node.isNiceText()){
      out.print(node.getText)
    }

    node.foreach((child) => DFS(child))
  }

  def setOutputStream(outStream: PrintStream) = {
    out = outStream
  }

  private var out: PrintStream = outStream
  private val root: NodeBase = treeRoot
}
