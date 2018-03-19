package compiler.ir


import util.SimpleFactory
import util.logging.Logger._
import compiler.ast.NodeBase
import compiler.ast.nodes._
import compiler.ir.nodes._

class DefaultIRFactory extends SimpleFactory[Option[NodeBase], Option[List[IRBuilder]]] {

  override def create(root : Option[NodeBase]): Option[List[IRBuilder]] = {
    root match{
      case Some(root) => {
        var irLst: List[IRBuilder] = List()

        var fEnv = getFunctionEnv(root)

        //note parallel execution
        try{
          root.par.map((func) =>{
            val irBuilder = new IRBuilder()
            irBuilder.setFunctionEnv(fEnv)
            genIRDFS(func,irBuilder)
            irBuilder
          }).toList.foreach((func) => irLst = irLst :+ func)
        }
        catch{
          case e: Exception => {
            println(s"IR Generation Failed with ${e.getMessage()}")
            println(s"STACK Trace:\n${e.getStackTrace().foldLeft("")((str,x) => str + x + "\n")}")
            None
          }
        }

        if(irLst.size > 0){
          Some(irLst)
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

  private def genIRDFS(node: NodeBase,irBuilder: IRBuilder): Option[IRTemporaryInstruction] ={
    node match{
      case n: FunctionNode => {

        //build parameter temps
        genIRDFS(n.getFunctionDeclaration(), irBuilder)

        // local var temps
        n.getFunctionBody().filter((child) => {
          child match{
            case c: VariableDeclarationNode => true
            case _ => false
          }
        }).map((vD) => irBuilder.newTemporary(IRType.fromASTType(vD.getType()), vD.asInstanceOf[VariableDeclarationNode].getName(),
          vD.asInstanceOf[VariableDeclarationNode].getTypeNode().getLen() )).toList

        // statments
        val stmtLst = n.getFunctionBody().filter((child) => {
          child match{
            case c: VariableDeclarationNode => false
            case _=> true
          }
        }).map((stmt) => genIRDFS(stmt,irBuilder)).toList
        None
      }
      case n: FunctionDeclarationNode => {
        val pNode = n.getParameterNode()
        var argLst: List[IRType.Type] = List()

        pNode match{
          case Some(pn) => {
            pn.foreach((arg) => {
              arg match {
                case arg: FunctionArgNode => {
                  argLst = argLst :+ IRType.fromASTType(arg.getType())
                  irBuilder.newTemporary(IRType.fromASTType(arg.getType()), arg.getName())
                }
              }
            })
          }
          case None => {}
        }

        irBuilder.setFunctionDeclaration(new IRFunctionInstruction(n.getName(),argLst,IRType.fromASTType(n.getType())))
        None
      }
      case n: AssignStatementNode => {
        var exp: NodeBase = null
        var tmpTarget: Option[IRTemporaryInstruction] = None
        var tmpExp: Option[IRTemporaryInstruction] = None

        if(n.size == 2){
          tmpExp = genIRDFS(n.getChild(1),irBuilder)

          irBuilder.setDref()
          tmpTarget = genIRDFS(n.getChild(0),irBuilder)
          irBuilder.clearDref()
        }
        else {
          tmpExp = genIRDFS(n.getChild(0),irBuilder)
          if(tmpExp != None){
            tmpTarget = Some(irBuilder.newTemporary(tmpExp.get.getType()))
          }
        }

        if(tmpExp != None){
          val aStmt = new IRAssignInstruction(tmpTarget.get,new IRExpression(Some(IROperator.newNop()),tmpExp))
          irBuilder.addInstruction(aStmt)
          tmpTarget
        }
        else {
          None
        }
      }
      case n: ReturnStatementNode => {
        if(n.size > 0){
          var retTmp = genIRDFS(n.getChild(0), irBuilder)
          irBuilder.addInstruction(new IRReturnInstruction(retTmp))
          None
        }
        else {
          irBuilder.addInstruction(new IRReturnInstruction(None))
          None
        }
      }
      case n: PrintStatementNode => {
        if(n.size > 0){
          var printTarget = genIRDFS(n.getChild(0), irBuilder)
          irBuilder.addInstruction(new IRPrintInstruction(printTarget, IRPrintInstruction.printTypeFromString(n.getText)))
          None
        }
        else {
          irBuilder.addInstruction(new IRPrintInstruction(None,IRPrintInstruction.printTypeFromString(n.getText)))
          None
        }
      }
      case whileStmt: WhileStatementNode => {
        val condition = whileStmt.getConditionExp()
        val jmpLabelTop = irBuilder.newLabel()

        irBuilder.addInstruction(jmpLabelTop)
        val conditionTemp = genIRDFS(condition, irBuilder).get
        val invTmp = irBuilder.newTemporary(conditionTemp.getType())

        irBuilder.addInstruction(new IRAssignInstruction(invTmp,new IRExpression(Some(new IROperator(IROperator.NOT,invTmp.getType())),Some(conditionTemp))))
        val jmpLabel = irBuilder.newLabel()
        irBuilder.addInstruction(new IRGoToInstruction(Some(invTmp), jmpLabel))

        genIRDFS(whileStmt.getBlock(), irBuilder)
        irBuilder.addInstruction(new IRGoToInstruction(None,jmpLabelTop))

        irBuilder.addInstruction(jmpLabel)
        None
      }
      case n: StatementNode => {
        // if statement
        val ifStmt = n.getChild(0).asInstanceOf[IfStatementNode]
        val condition = ifStmt.getConditionExp()
        val conditionTemp = genIRDFS(condition, irBuilder).get
        val invTmp = irBuilder.newTemporary(conditionTemp.getType())

        irBuilder.addInstruction(new IRAssignInstruction(invTmp,new IRExpression(Some(new IROperator(IROperator.NOT,invTmp.getType())),Some(conditionTemp))))
        val jmpLabel = irBuilder.newLabel()
        irBuilder.addInstruction(new IRGoToInstruction(Some(invTmp), jmpLabel))

        genIRDFS(ifStmt.getBlock(), irBuilder)

        if(n.size > 1){
          //has else
          val elseStmt = n.getChild(1).asInstanceOf[ElseStatementNode]
          val jmpOverElse = irBuilder.newLabel()

          irBuilder.addInstruction(new IRGoToInstruction(None,jmpOverElse))
          irBuilder.addInstruction(jmpLabel)

          genIRDFS(elseStmt.getBlock(), irBuilder)
          irBuilder.addInstruction(jmpOverElse)
        }
        else {
          irBuilder.addInstruction(jmpLabel)
        }

        None
      }
      case n: ExpressionNode => {
        val tmpLst = n.map((c) => {
          c match {
            case c: OperationNode => {
              genIRDFS(c.getChild(0), irBuilder)
            }
            case _ => {
              genIRDFS(c, irBuilder)
            }
          }
        }).filter((c) => c != None).map((c) => c.get).toList

        if(tmpLst.size > 1){
          var temporary = irBuilder.newTemporary(getOperatorType(n.getChild(1).asInstanceOf[OperationNode],tmpLst(0).getType))
          val firstStmt = new IRAssignInstruction(temporary,new IRExpression(Some(tmpLst(0)),Some(IROperator.operatorFromAST(n.getChild(1).asInstanceOf[OperationNode],tmpLst(0).getType)), Some(tmpLst(1))))
          irBuilder.addInstruction(firstStmt)

          for( i <- 2 until tmpLst.size){
            var temporaryNew = irBuilder.newTemporary(getOperatorType(n.getChild(1).asInstanceOf[OperationNode],tmpLst(0).getType))
            irBuilder.addInstruction(
              new IRAssignInstruction(temporaryNew, new IRExpression(Some(temporary),Some(IROperator.operatorFromAST(n.getChild(1).asInstanceOf[OperationNode],tmpLst(0).getType)), Some(tmpLst(i))))
            )
            temporary = temporaryNew
          }

          Some(temporary)
        }
        else if(tmpLst.size == 1){
          Some(tmpLst(0))
        }
        else {
          None
        }
      }
      case n: AtomLiteralNode =>{
        val literal = getLiteralFromString(n.getText)
        val temp = irBuilder.newTemporary(literal.getType())
        val aliteral = new IRAssignLiteralInstruction(temp,literal)
        irBuilder.addInstruction(aliteral)
        Some(temp)
      }
      case n: AtomVariableReferenceNode => {
        if(!n.isDereference()){
          irBuilder.lookupTemporary(n.getName())
        }
        else {
          val expTmp = genIRDFS(n.getExpression(),irBuilder)
          val myTmp = irBuilder.lookupTemporary(n.getName()).get.getCopy()
          myTmp.setDereferenceTemp(expTmp.get)

          if(irBuilder.isDref()){
            Some(myTmp)
          }
          else {
            val newTmp = irBuilder.newTemporary(myTmp.getType())
            irBuilder.addInstruction(new IRAssignInstruction(newTmp, new IRExpression(Some(IROperator.newNop()), Some(myTmp))))
            newTmp.stripType()
            Some(newTmp)
          }
        }
      }
      case n: AtomFunctionCallNode => {
        val funcNode = irBuilder.lookupFunction(n.getName()).get
        val fType = IRType.fromASTType(funcNode.getFunctionDeclaration().getType())

        val tmpLst = n.getArgList().map((arg) => {
          genIRDFS(arg, irBuilder).get
        }).toList

        if(fType != IRType.V){
          val targTmp = irBuilder.newTemporary(fType)
          irBuilder.addInstruction(new IRFunctionCallInstruction(n.getName(),tmpLst,Some(targTmp)))
          Some(targTmp)
        }
        else {
          irBuilder.addInstruction(new IRFunctionCallInstruction(n.getName(),tmpLst,None))
          None
        }
      }
      case n: BlockNode => {
        n.foreach((c) => genIRDFS(c,irBuilder))
        None
      }
      case n: NodeBase => {
        None
      }
    }
  }

  private def getOperatorType(n: OperationNode, operandType: IRType.Type): IRType.Type = {
    n match {
      case n: OperationAddNode => operandType
      case n: OperationSubNode => operandType
      case n: OperationMultNode => operandType
      case n: OperationEqualNode => IRType.Z
      case n: OperationLessNode => IRType.Z
    }
  }

  private def getLiteralType(str: String): IRType.Type = {
    val floatR = raw"\d+\.\d+".r
    val intR = raw"\d+".r
    val boolR = raw"(true|false)".r
    val charR = raw"'[a-zA-Z]'".r

    str match{
      case floatR(_*) => IRType.F
      case intR(_*) => IRType.I
      case boolR(_*) => IRType.Z
      case charR(_*) => IRType.C
      case _ => IRType.U
    }
  }

  private def getLiteralFromString(str: String): IRConst[_] = {
    val typ = getLiteralType(str)
    typ match{
      case IRType.I => new IRConst[Int](typ,str.toInt)
      case IRType.F => new IRConst[Float](typ,str.toFloat)
      case IRType.Z => new IRConst[Boolean](typ,str.toBoolean)
      case IRType.C => new IRConst[String](typ,str)
      case IRType.U => new IRConst[String](typ,str)
    }
  }

}
