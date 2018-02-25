package compiler.typechecker

import org.antlr.runtime._

import util.SimpleFactory
import compiler.ast.NodeBase
import compiler.ast.ASTType
import compiler.ast.nodes._
import util.logging.Logger._
import util.General._
import frontend.ui.TreePrinter

//TODO replace output string with parser object of some sort !!!
class DefaultTypeCheckerFactory extends SimpleFactory[ulNoActionsParser, Boolean] {
  override def create(parser: ulNoActionsParser): Boolean = {
    try{
      logMsg("invoking antlr parser and lexer", Level.INFO)
      var root: NodeBase = parser.program()

      logMsg("refactoring AST", Level.INFO)
      cleanAST(root)

      logMsg("Type Checking AST", Level.INFO)
      typeCheck(root)
    }
    catch{
      case e : RecognitionException => {
        logMsg(s"Parsing error: $e", Level.CERROR)
        false
      }
    }
  }

  private def typeCheck(n: NodeBase): Boolean = {
    try{
      typeCheckDFS(n, Map(), getFunctionEnvironment(n))
      true
    }
    catch{
      case e: TypeCheckException =>{
        logMsg("Failed Type Checking with type check error : " + e.getMessage(), Level.CERROR)
        println(e.getMessage())
        false
      }
      case e : Exception =>{
        logMsg("Failed Type Checking with unknwon error: " + e.getMessage(), Level.CERROR)
        println(e.getMessage())
        false
      }
    }
  }

  private def typeCheckDFS(n: NodeBase, vEnv: Map[String,(ASTType.Type, CompTypeNode)], fEnv: Map[String,(ASTType.Type, FunctionDeclarationNode)]): ASTType.Type ={
    n match{
      case n: ElseStatementNode =>{
        ASTType.B
      }
      case n: ConditionalStatementNode =>{
        val con = n.getConditionExp()
        val conType = typeCheckDFS(con, vEnv, fEnv)
        if(conType != ASTType.B){
          throw new TypeCheckException(s"Condition must be of type Bool, ${ASTType.typeToString(conType)} given", n.getLineNumber())
        }
        ASTType.B
      }
      case n: PrintStatementNode =>{
        n.foreach((child) => {
          //TODO change
          if(!(ASTType.validPrintTypes contains typeCheckDFS(child, vEnv, fEnv))){
            throw new TypeCheckException(s"Print statment only accepts ${ASTType.typeToString(ASTType.validPrintTypes)}",n.getLineNumber())
          }
        })
        ASTType.S
      }
      case n: AssignStatementNode => {
        n.foldLeft((typeCheckDFS(n.getChild(0),vEnv,fEnv), getExpTypeLength(n.getChild(0),vEnv))) ((acc, c) => {
          if(typeCheckDFS(c, vEnv, fEnv) != acc._1 || getExpTypeLength(c,vEnv) != acc._2){
            throw new TypeCheckException(s"var of type ${ASTType.typeToString(acc._1)} cannot take on value: ${ASTType.typeToString(typeCheckDFS(c, vEnv, fEnv))}!", n.getLineNumber())
          }
          acc
        })._1
      }
      case n: ReturnStatementNode => {
        //get function return type
        val func = getEnclosingFunction(n)

        func match {
          case Some(func) => {
            val fRetType = func.getFunctionDeclaration().getType()
            n.foreach((child)=> {
              if(typeCheckDFS(child,vEnv,fEnv) != fRetType || getExpTypeLength(child,vEnv) != func.getFunctionDeclaration().getLen() ){
                throw new TypeCheckException(s"Return with type ${ASTType.typeToString(typeCheckDFS(child,vEnv,fEnv))} does not match function return type: ${ASTType.typeToString(fRetType)}", n.getLineNumber())
              }
            })
            fRetType
          }
          case None => {
            throw new Exception ("Malformed AST! return statment outside of function!?!?!??!?!")
          }
        }
      }
      case n: ExpressionNode => {
        if(!n.isLeaf()){
          val res = n.foldLeft(typeCheckDFS(n.getChild(0),vEnv,fEnv))((acc,c) => {
              c match{
                case c: OperationNode => {
                  val allowable = getValidTypesForOp(c)
                  if(!(allowable contains acc)){
                    throw new TypeCheckException(s"type ${ASTType.typeToString(acc)} is not allowed in operation ${c.getOperator()}", n.getLineNumber())
                  }
                  val opType = typeCheckDFS(c, vEnv, fEnv)
                  if(opType != acc){
                    throw new TypeCheckException(s"operator: ${c.getOperator()} cannot be applied to types: ${ASTType.typeToString(acc)}, ${ASTType.typeToString(opType)}",n.getLineNumber())
                  }

                  getOperatorOutputType(acc,opType,c)
                }
                case c: NodeBase => {
                  if(typeCheckDFS(c,vEnv,fEnv) != acc){
                    logMsg(s"type: ${ASTType.typeToString(typeCheckDFS(c,vEnv,fEnv))} does not match ${ASTType.typeToString(acc)}", Level.ERROR)
                    ASTType.BAD
                  }
                  else{
                    acc
                  }
                }
              }
            });

          if(res != ASTType.BAD){
            //pass
            res
          }
          else{
            throw new TypeCheckException(s"All elements of an expression must have the same type", n.getLineNumber())
          }

        }
        else {
          logMsg("Internal Error, AST malformed!", Level.CERROR)
          throw new TypeCheckException("malformed AST", 0)
        }
      }
      case n: FunctionNode =>{
        val declaration = n.getFunctionDeclaration()
        val body = n.getFunctionBody()
        var localEnv = vEnv

        //build local environment
        body.foreach((child) => {
          child match {
            case c: VariableDeclarationNode => {
              if(localEnv contains c.getName()){
                throw new TypeCheckException(s"${c.getName()} Rediclared!", c.getLineNumber())
              }
              else if((fEnv - declaration.getName()) contains c.getName()){
                throw new TypeCheckException(s"${c.getName()} conflicts with function of same name!",c.getLineNumber())
              }
              else if(c.getType == ASTType.V){
                throw new TypeCheckException(s"Cannot declar variable of type void",c.getLineNumber())
              }
              localEnv = localEnv + (c.getName() -> (c.getType(),c.getTypeNode()))
            }
            case _ => {}
          }
        })

        val fParam = declaration.getParameterNode()
        fParam match{
          case Some(fp) =>{
            fp.foreach((param) => {
              param match {
                case p: FunctionArgNode =>{
                  if(localEnv contains p.getName()){
                    throw new TypeCheckException(s"${p.getName()} Rediclared!", p.getLineNumber())
                  }
                  else if((fEnv - declaration.getName()) contains p.getName()){
                    throw new TypeCheckException(s"${p.getName()} conflicts with function of same name!",p.getLineNumber())
                  }
                  else if(p.getType == ASTType.V){
                    throw new TypeCheckException(s"Cannot declar variable of type void",p.getLineNumber())
                  }
                  localEnv = localEnv + (p.getName() -> (p.getType(), p.getTypeNode()))
                }
                case _ => {}
              }
            })
          }
          case None => {}
        }

        //run statments under environment
        body.foreach((child) => {
          child match {
            case c: VariableDeclarationNode => {}
            case c: StatementNode => typeCheckDFS(c,localEnv,fEnv)
          }
        })
        ASTType.S
      }
      case n: AtomVariableReferenceNode => {
        dereferenceVar(n,vEnv)
      }
      case n: AtomFunctionCallNode =>{
        val funcData = fEnv get n.getName()
        funcData match {
          case Some(ft) => {
            //check that args are correct
            val fArgs = n.getArgList()
            val fParams = ft._2.getParameterNode()

            fParams match{
              case Some(fp) => {
                if(fp.size == fArgs.size){
                  fp.toList.zip(fArgs).foreach((argParam) => {
                    argParam match{
                      case (p, a) => {
                        if(typeCheckDFS(a,vEnv,fEnv) != p.getType()){
                          throw new TypeCheckException(s"Function: ${n.getName()} expected ${ASTType.typeToString(p.getType())} got ${ASTType.typeToString(typeCheckDFS(a,vEnv,fEnv))}", n.getLineNumber())
                        }
                      }
                    }
                  })
                }
                else{
                  throw new TypeCheckException(s"Function: ${n.getName()} takes ${fp.size} arguments, ${fArgs.size} given",n.getLineNumber())
                }
              }
              case None => {
                if(fArgs.size > 0){
                  throw new TypeCheckException(s"Function: ${n.getName()} takes 0 arguments, ${fArgs.size} given", n.getLineNumber())
                }
              }
            }

            ft._1 // function type
          }
          case None => throw new TypeCheckException(s"Function: ${n.getName()} not declared!", n.getLineNumber())
        }
      }
      case n: NodeBase => {
        var out = n.getType()
        if(!n.isLeaf()){
          n.foreach((c) =>{
            val typeN = typeCheckDFS(c,vEnv,fEnv)
            if(typeN != ASTType.BAD){
              out = typeN
            }
          })
        }
        out
      }
    }
  }

  //apply dereferencing to the type of vRef if possible / insure no array - non array miss match
  private def dereferenceVar(vRef: AtomVariableReferenceNode, vEnv: Map[String,(ASTType.Type,CompTypeNode)]): ASTType.Type = {
    val vType = vEnv get vRef.getName()
    vType match{
      case Some((vt,vt2)) => {

        if(!ASTType.isArrayType(vt) && vRef.isDereference()){
          throw new TypeCheckException(s"${vRef.getName()} is an not an array",vRef.getLineNumber())
        }
        else {
          if(ASTType.isArrayType(vt) && vRef.isDereference()){
            ASTType.drefMap getOrElse (vt, ASTType.BAD)
          }
          else {
            vt
          }
        }
      }
      case None => throw new TypeCheckException(s"Varialble ${vRef.getName()} is undefined! ", vRef.getLineNumber())
    }
  }

  private def getFunctionEnvironment(root: NodeBase): Map[String,(ASTType.Type, FunctionDeclarationNode)] = {
    var functionEnv = Map[String,(ASTType.Type,FunctionDeclarationNode)]()

    root.foreach((function) => {
      function match{
        case f: FunctionNode => {
          val fDec = f.getFunctionDeclaration()
          functionEnv = functionEnv + (fDec.getName() -> (fDec.getType(), fDec))
        }
        case _ => {
          throw new Exception("Malformed AST! root noded has non FunctionNode child!!!!!");
        }
      }
    })

    if(!(functionEnv contains "main")){
      throw new TypeCheckException("\"main\" function not declared!",0)
    }
    functionEnv
  }

  private def getEnclosingFunction(n: NodeBase): Option[FunctionNode] ={
    val parent = n.getParent()
    parent match{
      case Some(p) => {
        p match{
          case p: FunctionNode => Some(p)
          case p: NodeBase => getEnclosingFunction(p)
        }
      }
      case None => None
    }
  }

  private def getValidTypesForOp(op: OperationNode): List[ASTType.Type] = {
    op match{
      case op: OperationAddNode => ASTType.validAddTypes
      case op: OperationSubNode => ASTType.validSubTypes
      case op: OperationEqualNode => ASTType.validEqualTypes
      case op: OperationLessNode => ASTType.validLessTypes
      case op: OperationMultNode => ASTType.validMultTypes
    }
  }

  private def getOperatorOutputType(l: ASTType.Type, r: ASTType.Type, op: OperationNode): ASTType.Type = {
    op match{
      case op: OperationAddNode => l
      case op: OperationSubNode => l
      case op: OperationEqualNode => ASTType.B
      case op: OperationLessNode => ASTType.B
      case op: OperationMultNode => l
    }
  }


  private def getExpTypeLength(n: NodeBase, vEnv: Map[String,(ASTType.Type, CompTypeNode)]): Int ={
    n.foldLeft(0)((acc,child) => {
      child match{
        case c: AtomVariableReferenceNode => {
          if(!c.isDereference()){
            val typeLookUp = (vEnv get c.getName())
            typeLookUp match{
              case Some(tlp) => tlp._2.getLen()
              case None => acc
            }
          }
          else{
            acc
          }
        }
        case c: NodeBase => {
          val len = getExpTypeLength(c,vEnv)
          if(len != 0){
            len
          }
          acc
        }
      }
    })
  }
}
