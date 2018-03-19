package compiler.ir

import compiler.ast.ASTType

object IRType extends Enumeration {

  def fromASTType(astTyp: ASTType.Type): Type ={
    astTypMap.get(astTyp).getOrElse(throw new IRException("Type Mapping Failed!"))
  }

  def typeToString(typ: Type): String = {
    typ match {
      case I => "I"
      case F => "F"
      case B => "B"
      case C => "C"
      case U => "U"
      case Z => "Z"
      case V => "V"
      case AI => "AI"
      case AF => "AF"
      case AB => "AB"
      case AC => "AC"
      case AU => "AU"
      case AZ => "AZ"
      case ARG => "ARG"
      case BAD => "BAD"
    }
  }

  def removeArrayType(typ: Type): Type ={
    typ match{
      case AI => I
      case AF => F
      case AB => B
      case AC => C
      case AU => U
      case AZ => Z
      case _ => typ 
    }
  }

  def typeToString(typLst: List[Type]): String ={
    typLst.foldLeft("")((acc,t) => acc + typeToString(t))
  }

  type Type = Value
  val I, F, B, C, U, Z, V, AI, AF, AB, AC, AU, AZ, ARG, BAD= Value

  //mapping between AST and IR types
  val astTypMap = Map(ASTType.I -> I,
                      ASTType.F -> F,
                      ASTType.B -> Z,
                      ASTType.C -> C,
                      ASTType.S -> U,
                      ASTType.V -> V,
                      ASTType.AI -> AI,
                      ASTType.AF -> AF,
                      ASTType.AB -> AZ,
                      ASTType.AC -> AC,
                      ASTType.AS -> AU,
                      ASTType.BAD -> BAD)
}
