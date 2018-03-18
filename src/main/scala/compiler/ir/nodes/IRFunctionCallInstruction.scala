package compiler.ir.nodes

import compiler.ir._


class IRFunctionCallInstruction(name: String,aLst: List[IRTemporaryInstruction],retTmp: IRTemporaryInstruction) extends IRInstructionBase {

  override def toString(): String ={
    val argString = argList.foldLeft("")((str,x) => str + s"T${x.getId()},").dropRight(1)
    s"T${targetTmp.getId()} := CALL ${fName}(${argString});\n"
  }

  private var fName: String = name
  private var argList: List[IRTemporaryInstruction] = aLst
  private var targetTmp: IRTemporaryInstruction = retTmp;
}
