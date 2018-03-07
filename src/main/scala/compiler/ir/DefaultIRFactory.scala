package compiler.ir


import util.SimpleFactory
import util.logging.Logger._
import compiler.ast.NodeBase
import compiler.ast.nodes._
import compiler.ir.nodes._

class DefaultIRFactory extends SimpleFactory[Option[NodeBase], Option[IRNodeBase]] {

  override def create(root : Option[NodeBase]): Option[IRNodeBase] = {
    root match{
      case Some(root) => {
        var irRoot = new IRNodeBase()

        var fEnv = getFunctionEnv(root)

        //note parallel execution
        try{
          root.par.map((func) =>{
            genIRDFS(func,Map[String,TemporaryNode](),fEnv)
          }).toList.foreach((func) => irRoot.addChild(func))
        }
        catch{
          case e: Exception => {
            println(s"IR Generation Failed with Error: ${e.getMessage()}")
            None
          }
        }

        if(irRoot.size > 0){
          Some(irRoot)
        }
        else{
          None
        }
      }
      case None =>{
        None
      }
    }

  }

  private def getFunctionEnv(root: NodeBase): Map[String,FunctionNode] ={
    root.foldLeft(Map[String,FunctionNode]())((mp,func) =>{
      val funcN = func.asInstanceOf[FunctionNode]
      mp + (funcN.getFunctionDeclaration().getName() -> funcN)
    })
  }

  private def genIRDFS(node: NodeBase,vEnv: Map[String,TemporaryNode], fEnv: Map[String,FunctionNode]): IRNodeBase ={
    new IRNodeBase()
  }
}
