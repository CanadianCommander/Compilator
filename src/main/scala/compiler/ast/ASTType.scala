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

  def isArrayType(t: Type): Boolean = {
    arrayTypes contains t
  }

  //Int, Float, Char, String, Bool, Void
  val I, F, C, S, B, V, AI, AF, AC, AS, AB, BAD= Value
  val validPrintTypes = List(I,F,C,S,B)
  val arrayTypes = List(AI,AF,AC,AS,AB)
  val drefMap    = Map(AI -> I, AF -> F, AC -> C, AS -> S, AB -> B)
}
