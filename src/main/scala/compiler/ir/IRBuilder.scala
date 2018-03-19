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

  def newTemporary(typ: IRType.Type, vName: String = "NaN", len: Int = 0): IRTemporaryInstruction = {
    var newTmp = new IRTemporaryInstruction(getNextTempId(),typ,vName)
    irTemps = irTemps :+ newTmp

    if(len != 0){
      addInstruction(new IRNewArrayInstruction(newTmp,len))
    }

    newTmp
  }

  def newLabel():IRLabelInstruction ={
    new IRLabelInstruction(getNextLabelId())
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

  private var idTrk: Int = -1
  private def getNextTempId(): Int = {
    idTrk += 1
    idTrk
  }

  private var idLabel: Int = -1
  private def getNextLabelId(): Int = {
    idLabel += 1
    idLabel
  }


  override def toString(): String = {
    var str = ""
    str = irFunctionDeclaration.get.toString()
    str = str + "{\n"
    str = irTemps.foldLeft(str)((acc,t) => acc + t.toString())
    str = irInstructions.foldLeft(str)((acc,i) => acc + i.toString())

    //inject return statement if one is not present
    try{
      irInstructions.last match{
        case lst: IRReturnInstruction => {}
        case lst: IRInstructionBase =>{
          str = str + "RETURN;\n"
        }
      }
    }
    catch{
      case e: Exception => {//list must be empty
      }
    }

    str = str + "}\n"
    str
  }

  def setDref() = enableDref = true
  def clearDref() = enableDref = false
  def isDref(): Boolean = enableDref

  private var enableDref: Boolean = false
  private var functionEnv: Map[String,FunctionNode] = Map()
  private var irTemps: List[IRTemporaryInstruction] = List()
  private var irInstructions: List[IRInstructionBase] = List()
  private var irFunctionDeclaration: Option[IRFunctionInstruction] = None
}
