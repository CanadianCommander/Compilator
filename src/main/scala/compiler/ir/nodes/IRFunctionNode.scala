package compiler.ir.nodes

import compiler.ir._
import compiler.ast.nodes.FunctionArgNode
import compiler.ast.nodes.FormalParamsNode
import compiler.ast.nodes.FunctionDeclarationNode


class IRFunctionNode(funcDec: FunctionDeclarationNode) extends IRNodeBase {

  //called at bottom of class
  def construct(funcDec: FunctionDeclarationNode) = {
    setType(IRType.fromASTType(funcDec.getType()))
    setName(funcDec.getName())
    //get params
    val fParams = funcDec.getParameterNode()
    fParams match {
      case Some(fp) => {
        fp.foreach((arg) => {
          arg match {
            case arg: FunctionArgNode => {
              argLst = argLst :+ IRType.fromASTType(arg.getType())
            }
            case _ => {
              throw new IRException(s"Function Argument not of type FunctionArgNode! AST incorrect! ${arg.getClass}")
            }
          }
        })
      }
      case None => {}
    }
  }

  def getName(): String = myName
  def setName(nName: String) = myName = nName

  override def toString(): String = {
    s"FUNC ${myName} (${IRType.typeToString(argLst)})${getType()};\n" + super.toString()
  }

  protected var myName: String = ""
  protected var argLst: List[IRType.Type] = List()

  //call constructor function
  construct(funcDec)
}
