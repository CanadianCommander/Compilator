package compiler.ast

import util.logging.Logger._


object ASTType extends Enumeration {
  type Type = Value

  def javaCompat(t: String): Type = {
    var txt = t.toUpperCase()
    if(txt == "INT"){
      I
    }
    else if (txt == "FLOAT"){
      F
    }
    else if (txt == "CHAR"){
      C
    }
    else if (txt == "STRING"){
      S
    }
    else if (txt == "BOOL"){
      B
    }
    else if (txt == "BOOLEAN"){
      B
    }
    else if (txt == "AINT"){
      AI
    }
    else if (txt == "AFLOAT"){
      AF
    }
    else if (txt == "ACHAR"){
      AC
    }
    else if (txt == "ASTRING"){
      AS
    }
    else if (txt == "ABOOLEAN"){
      AB
    }
    else if (txt == "BOOL"){
      AB
    }
    else if (txt == "VOID"){
      V
    }
    else{
      logMsg(s"bad Type Lookup t(${t} ${txt}) does not name a type", Level.ERROR)
      throw new Exception("t does not name a type!")
    }
  }

  def typeToString(t: Type): String = {
    t match {
      case I => "Int"
      case F => "Float"
      case C => "Char"
      case S => "String"
      case B => "Bool"
      case AI => "Int Array"
      case AF => "Float Array"
      case AC => "Char Array"
      case AS => "String Array"
      case AB => "Bool Array"
      case V => "Void"
      case _    => "BAD"
    }
  }

  def typeToString(t: List[Type]): String ={
    var sOut = ""
    t.foreach((typ) => {
      sOut += typeToString(typ)
    })
    sOut 
  }

  def isArrayType(t: Type): Boolean = {
    arrayTypes contains t
  }

  //Int, Float, Char, String, Bool, Void, Array Int, Array Float, Array Char, Array String, Array Bool, bad value
  val I, F, C, S, B, V, AI, AF, AC, AS, AB, BAD= Value
  val arrayTypes = List(AI,AF,AC,AS,AB)
  val drefMap    = Map(AI -> I, AF -> F, AC -> C, AS -> S, AB -> B)

  //statments
  val validPrintTypes = List(I,F,C,S,B)

  //operators
  val validAddTypes = List(I,F,C,S)
  val validSubTypes = List(I,F,C)
  val validMultTypes = List(I,F)
  val validLessTypes = List(I,F,C,S,B)
  val validEqualTypes = List(I,F,C,S,B)

}
