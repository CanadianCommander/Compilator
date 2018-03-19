package compiler.ir.nodes

import compiler.ir._


class IRFunctionCallInstruction(name: String,aLst: List[IRTemporaryInstruction],retTmp: Option[IRTemporaryInstruction]) extends IRInstructionBase {

  override def toString(): String ={
    val argString = argList.foldLeft("")((str,x) => str + s"T${x.getId()} ")
    targetTmp match{
      case Some(tt) => {
        s"${tt.getString()} := CALL ${fName}(${argString});\n"
      }
      case None => {
        s"CALL ${fName}(${argString});\n"
      }
    }
  }

  private var fName: String = name
  private var argList: List[IRTemporaryInstruction] = aLst
  private var targetTmp: Option[IRTemporaryInstruction] = retTmp;
}
