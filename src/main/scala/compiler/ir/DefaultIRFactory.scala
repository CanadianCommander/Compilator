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
          root.map((func) =>{
            genIRDFS(func,Map[String,IRTemporaryNode](),fEnv, new IRNodeFactory())
          }).toList.foreach((func) => irRoot.addChild(func))
        }
        catch{
          case e: Exception => {
            println(s"IR Generation Failed with ${e.getMessage()}")
            None
          }
        }

        if(irRoot.size > 0){
          print(irRoot)
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

  private def genIRDFS(node: NodeBase,vEnv: Map[String,IRTemporaryNode], fEnv: Map[String,FunctionNode], irFac : IRNodeFactory): IRNodeBase ={
    node match{
      case n: FunctionNode => {
        val functionRoot = irFac.create(n.getFunctionDeclaration())

        //build parameter temps
        val paramTemps = genIRDFS(n.getFunctionDeclaration(), vEnv,fEnv,irFac)

        // local var temps
        val tmpLst = n.getFunctionBody().filter((child) => {
          child match{
            case c: VariableDeclarationNode => true
            case _ => false
          }
        }).map((vN) => genIRDFS(vN,vEnv,fEnv,irFac)).toList
        val localTemps = new IRNodeBase()
        localTemps.addChild(tmpLst)

        // statments
        var localEnv = appendTempToEnv(localTemps.toList, appendTempToEnv(paramTemps.toList,vEnv))
        val stmtLst = n.getFunctionBody().filter((child) => {
          child match{
            case c: VariableDeclarationNode => false
            case _=> true
          }
        }).map((stmt) => genIRDFS(stmt,localEnv,fEnv,irFac)).toList

        functionRoot.addChild(paramTemps)
        functionRoot.addChild(localTemps)
        functionRoot.addChild(stmtLst)
        functionRoot
      }
      case n: FunctionDeclarationNode => {
        val fParams = n.getParameterNode()

        fParams match{
          case Some(fp) => {
            var out = new IRNodeBase()
            fp.foreach((arg) => {
              out.addChild(irFac.create(arg))
            })
            out
          }
          case None => new IRNodeBase()
        }
      }
      case n: AssignStatementNode => {
        var statementNode = irFac.create(n).asInstanceOf[IRStatementNode]

        if(n.size == 2){
          //assignment statement
          statementNode.setTarget(genIRDFS(n.getChild(0),vEnv,fEnv,irFac))
          statementNode.setExpression(genIRDFS(n.getChild(1),vEnv,fEnv,irFac))
        }
        else {
          //nop statement

        }

        statementNode
      }
      case n: AtomLiteralNode =>{
        irFac.create(n)
      }
      case n: AtomNode => {
        irFac.newTemporary(IRType.I)
      }
      case n: ExpressionNode => {
        expressionToIR(n,vEnv,fEnv,irFac)
      }
      case n: NodeBase =>{
        irFac.create(n)
      }
    }
  }

  def appendTempToEnv(tmp: IRTemporaryNode, vEnv: Map[String,IRTemporaryNode]): Map[String,IRTemporaryNode] = {
    vEnv + (tmp.getVarName() -> tmp)
  }

  def appendTempToEnv(tmpl: List[IRNodeBase], vEnv: Map[String,IRTemporaryNode]): Map[String,IRTemporaryNode] = {
    tmpl.foldLeft(Map[String,IRTemporaryNode]())((acc,t) => appendTempToEnv(t.asInstanceOf[IRTemporaryNode],acc))
  }

  def expressionToIR(exp: ExpressionNode, vEnv: Map[String,IRTemporaryNode], fEnv: Map[String,FunctionNode], irFac : IRNodeFactory): IRNodeBase = {
    exp.toList match {
      case l :: op :: rest => {
        val left = genIRDFS(l,vEnv,fEnv,irFac)
        val right = genIRDFS(op.getChild(0),vEnv,fEnv,irFac)
        val operation = irFac.create(op).asInstanceOf[IROperatorNode]

        val newExp = new IRExpressionNode(Some(left),Some(right),Some(operation))
        newExp.assignTemporary(irFac.newTemporary(newExp.getType()))
        val res = irFac.newTemporary(newExp.getType())

        new IRStatementNode(Some(res), Some(newExp))
      }
      case l :: Nil => {
        val left = genIRDFS(l,vEnv,fEnv,irFac)
        val newExp = new IRExpressionNode(Some(left),None,None)
        newExp.assignTemporary(irFac.newTemporary(newExp.getType()))
        new IRStatementNode(Some(irFac.newTemporary(newExp.getType())),Some(newExp))
      }
      case _ => throw new IRException("WTF I die Now!")
    }
  }

}
