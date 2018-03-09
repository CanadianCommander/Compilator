package compiler.ir

import util.SimpleFactory
import compiler.ast.nodes._
import compiler.ast.NodeBase
import compiler.ir.nodes._

class IRNodeFactory extends SimpleFactory[NodeBase,IRNodeBase] {

  override def create(arg: NodeBase): IRNodeBase ={
    arg match{
      case arg: OperationNode => {
        val opN = new IROperatorNode(arg)
        opN.assignTemporary(newTemporary(opN.getType()))
        opN
      }
      case arg: AtomLiteralNode => {
        val constN = EzMk.IRConstNode(arg)
        constN.assignTemporary(newTemporary(constN.getType()))
        constN
      }
      case arg: AssignStatementNode => {
        new IRStatementNode()
      }
      case arg: StatementNode => {
        new IRNodeBase()/// DELETE ME
      }
      case arg: FunctionDeclarationNode => {
        new IRFunctionNode(arg)
      }
      case arg: FunctionArgNode => {
        new IRTemporaryNode(getNextTmpId(),IRType.fromASTType(arg.getType()),arg.getName())
      }
      case arg: VariableDeclarationNode => {
        new IRTemporaryNode(getNextTmpId(),IRType.fromASTType(arg.getType()),arg.getName())
      }
      case arg: NodeBase => {
        throw new IRException(s"no conversion defined for ${arg.getClass}")
      }
    }
  }

  def newTemporary(tempT: IRType.Type): IRTemporaryNode ={
    new IRTemporaryNode(getNextTmpId(), tempT,"???")
  }

  private var idCtr = 0
  def getNextTmpId() = {
    idCtr += 1
    idCtr
  }

}
