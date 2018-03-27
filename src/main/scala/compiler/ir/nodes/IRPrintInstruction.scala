package compiler.ir.nodes

import compiler.ir._


class IRPrintInstruction(toPrint: Option[IRTemporaryInstruction], printT: IRPrintInstruction.printType) extends IRInstructionBase {

  override def toString(): String ={
    var strStart = ""
    if(myType == IRPrintInstruction.PRINT){
      strStart = "PRINT"
    }
    else {
      strStart = "PRINTLN"
    }

    printTarget match {
      case Some(pt) => {
         strStart + s"${IRType.typeToString(pt.getType())} ${pt.getString()};\n"
      }
      case None => {
        strStart + " ;\n"
      }
    }
  }

  def getTemporary(): Option[IRTemporaryInstruction] = printTarget
  def getPrintType(): IRPrintInstruction.printType = myType

  private var printTarget: Option[IRTemporaryInstruction] = toPrint
  private val myType: IRPrintInstruction.printType = printT
}

object IRPrintInstruction extends Enumeration{

  def printTypeFromString(str: String): printType = {
    str match{
      case "print" => PRINT
      case "println" => PRINT_LN
    }
  }

  type printType = Value
  val PRINT, PRINT_LN = Value
}
