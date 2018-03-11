package compiler.ir

import compiler.ir.nodes._
import compiler.ast.nodes.FunctionNode

class IRBuilder {


  def addInstruction(irInst: IRInstructionBase) = {
    irInstructions = irInstructions :+ irInst
  }

  def setFunctionDeclaration(irFdec: IRFunctionInstruction) = {
    irFunctionDeclaration = Some(irFdec)
  }

  def getInstructionList(): List[IRInstructionBase] = {
    irInstructions
  }

  def newTemporary(typ: IRType.Type, vName: String = "NaN"): IRTemporaryInstruction = {
    var newTmp = new IRTemporaryInstruction(getNextTempId(),typ,vName)
    irTemps = irTemps :+ newTmp
    newTmp
  }

  def lookupFunction(name: String): Option[FunctionNode] = {
    val func = functionEnv.find((c) => {
      if(c._1 == name) true
      else false
    })

    func match {
      case Some(f) => {
        Some(f._2)
      }
      case _ => None
    }
  }

  def lookupTemporary(name: String): Option[IRTemporaryInstruction] = {
    irTemps.find((t) => t.getVarName() == name)
  }

  def setFunctionEnv(fEnv: Map[String,FunctionNode]) = {
    functionEnv = fEnv
  }

  private var idTrk: Int = 0
  private def getNextTempId(): Int = {
    idTrk += 1
    idTrk
  }


  override def toString(): String = {
    var str = ""
    str = irFunctionDeclaration.get.toString()
    str = irTemps.foldLeft(str)((acc,t) => acc + t.toString())
    str = irInstructions.foldLeft(str)((acc,i) => acc + i.toString())
    str
  }

  private var functionEnv: Map[String,FunctionNode] = Map()
  private var irTemps: List[IRTemporaryInstruction] = List()
  private var irInstructions: List[IRInstructionBase] = List()
  private var irFunctionDeclaration: Option[IRFunctionInstruction] = None
}
