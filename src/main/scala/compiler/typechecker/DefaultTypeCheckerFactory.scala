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

  private def typeCheckDFS(n: NodeBase, vEnv: Map[String,ASTType.Type], fEnv: Map[String,(ASTType.Type, FunctionDeclarationNode)]): ASTType.Type ={
    n match{
      case n: PrintStatementNode =>{
        n.foreach((child) => {
          //TODO change
          if(!(ASTType.validPrintTypes contains typeCheckDFS(child, vEnv, fEnv))){
            throw new TypeCheckException("Print statment only accepts Strings",n.getLineNumber())
          }
        })
        ASTType.S
      }
      case n: ExpressionNode => {
        if(!n.isLeaf()){
          val res = n.foldLeft(typeCheckDFS(n.getChild(0),vEnv,fEnv))((acc,c) => {
              if(typeCheckDFS(c,vEnv,fEnv) != acc){
                logMsg(s"type: ${ASTType.typeToString(typeCheckDFS(c,vEnv,fEnv))} does not match ${ASTType.typeToString(acc)}", Level.ERROR)
                ASTType.BAD
              }
              else{
                acc
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
                throw new TypeCheckException(s"${c.getName()} Rediclared!", n.getLineNumber())
              }
              localEnv = localEnv + (c.getName() -> c.getType())
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
                    throw new TypeCheckException(s"${p.getName()} Rediclared!", n.getLineNumber())
                  }
                  localEnv = localEnv + (p.getName() -> p.getType())
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
  private def dereferenceVar(vRef: AtomVariableReferenceNode, vEnv: Map[String,ASTType.Type]): ASTType.Type = {
    val vType = vEnv get vRef.getName()
    vType match{
      case Some(vt) => {
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

    functionEnv
  }
}
