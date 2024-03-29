package compiler.jasmin

import util.logging.Logger._
import util.SimpleFactory

import compiler.ir._
import compiler.ir.nodes._


class DefaultJasminFactory(className: String) extends SimpleFactory[Option[List[IRBuilder]], Option[String]] {

  override def create(builders: Option[List[IRBuilder]]): Option[String] = {
    builders match{
      case Some(build) => {
        logMsg("Building Jasmin asm", Level.INFO)

        var out = s".class public ${className}\n"
        out += ".super java/lang/Object\n"
        out += build.map((b) => irToJasmin(b)).reduce((acc,b) => acc + b)
        out += boilerplate

        logMsg("Jasmin generation Complete", Level.INFO)
        Some(out)
      }
      case None => {
        logMsg("Jasmin Factory not provided with IR code. Aborting", Level.CERROR)
        None
      }
    }
  }

  //build jasmin code for function
  def irToJasmin(build: IRBuilder): String = {
    val instructionList = build.getInstructionList()
    val temporaryList   = build.getTemporaryList()
    val functionDecl    = build.getFunctionDeclaration()
    var str = ""

    var fName = functionDecl.getName()
    if(fName == "main"){
      fName = "__main"
    }
    //function declaration
    str += s".method public static ${fName}(${typeToString(functionDecl.getArgs())})${typeToString(functionDecl.getReturnT())}\n"
    str += s".limit locals ${temporaryList.size}\n"
    //temporaryList.foreach((t) => {
      	//.var 0 is T0  I from L_0 to L_1
    //    str += s".var ${t.getId()} is T${t.getId} ${typeToString(t.getType())} from F_START to F_END \n"
    //})
    str += ".limit stack 100\n"
    str += "F_START: \n"

    if(!instructionList.isEmpty){
      instructionList.foreach((instruct) => {
        instruct match {
          case i: IRAssignLiteralInstruction => {
            val lit = i.getLiteral()
            val targ = i.getTemporary()
            val inPre = irTypeToInstructionPrefix(lit.getType())

            if(lit.getType() == IRType.Z){
              if(lit.getValue().asInstanceOf[Boolean]){
                str += "ldc 1 \n"
              }else {
                str += "ldc 0 \n"
              }
            }
            else if (lit.getType() == IRType.C){
              val myChar = lit.getValue().asInstanceOf[Char]
              str += s"ldc ${myChar.toInt} \n"
            }
            else {
              str += s"ldc ${lit.toString()}\n"
            }
            str += s"${inPre}store ${targ.getId()}\n"
          }
          case i: IRFunctionCallInstruction => {
            val funcName = i.getName()
            val argList = i.getArgList()
            val target = i.getTemporary()
            val retType = i.getReturnType()

            //push arguments on stack
            argList.foreach((a) => {
              if(isArrayType(a.getType())){
                str += s"aload ${a.getId()} \n"
              }
              else {
                str += s"${irTypeToInstructionPrefix(a.getType())}load ${a.getId()}\n"
              }
            })

            //call
            str += s"invokestatic ${className}/${funcName}(${typeToString(argList.map((a) => a.getType()))})${typeToString(retType)} \n"

            if(target.nonEmpty){
              str += s"${irTypeToInstructionPrefix(target.get.getType())}store ${target.get.getId()} \n"
            }
            else if(retType != IRType.V){
              //even though there is no varaible to receive the value we must still pop it else stack overflow may occure
              str += "pop \n"
            }

          }
          case i: IRGoToInstruction => {
            val con = i.getCondition()
            val targ = i.getJumpTarget()

            if(con.nonEmpty){
              //conditional jump
              str += s"iload ${con.get.getId()} \n"
              str += s"ifne L_${targ.getId()} \n"
            }
            else {
              //jump
              str += s"goto L_${targ.getId()} \n"
            }
          }
          case i: IRLabelInstruction => {
            str += s"L_${i.getId()}: \n"
          }
          case i: IRNewArrayInstruction => {
            val aSize = i.getArraySize()
            val tmp   = i.getTemporary()

            if(tmp.getType() == IRType.AU){
              str += s"ldc ${aSize} \n"
              str += s"anewarray java/lang/String\n"
              str += s"astore ${tmp.getId()} \n"
            }
            else {
              str += s"ldc ${aSize} \n"
              str += s"newarray ${irTypeToJavaArrayType(tmp.getType())} \n"
              str += s"astore ${tmp.getId()} \n"
            }
          }
          case i: IRPrintInstruction => {
            val target = i.getTemporary()

            var printStr = "print"
            if(i.getPrintType() == IRPrintInstruction.PRINT_LN){
              printStr = "println"
            }

            if(target.nonEmpty){
              if(target.get.getType() == IRType.C){
                str += "getstatic java/lang/System/out Ljava/io/PrintStream; \n"
                str += s"iload ${target.get.getId()} \n"
                str += "i2c \n"
                str += s"invokevirtual java/io/PrintStream/${printStr}(C)V \n"
              }
              else {
                str += "getstatic java/lang/System/out Ljava/io/PrintStream; \n"
                str += s"${irTypeToInstructionPrefix(target.get.getType())}load ${target.get.getId()} \n"
                str += s"invokevirtual java/io/PrintStream/${printStr}(${typeToString(target.get.getType())})V \n"
              }
            }
          }
          case i: IRReturnInstruction => {
            val retVal = i.getTemporary()

            if(retVal.nonEmpty){
              if(isArrayType(retVal.get.getType())){
                str += s"aload ${retVal.get.getId()} \n"
                str += s"areturn \n"
              }
              else {
                str += s"${irTypeToInstructionPrefix(retVal.get.getType())}load ${retVal.get.getId()} \n"
                str += s"${irTypeToInstructionPrefix(retVal.get.getType())}return \n"
              }
            }
            else {
              str += "return \n"
            }
          }
          case i: IRAssignInstruction => {
            val exp   = i.getExpression()
            val targ  = i.getTemporary()
            val op    = exp.getOperator()
            val right = exp.getRight().get

            val opType = op.getType()
            val opOperand = op.getOperation()

            var storeStr = s"${irTypeToInstructionPrefix(targ.getType())}store ${targ.getId()}\n"
            if(targ.isDereference()){
              //handle array derference special case
              val dTmp = targ.getDerefTemp()
              storeStr = s"aload ${targ.getId()} \n"
              storeStr += "swap \n"
              storeStr += s"iload ${dTmp.get.getId()} \n"
              storeStr += "swap \n"
              storeStr += s"${irTypeToInstructionPrefix(targ.getType())}store \n"
            }

            if(exp.getLeft() == None && right != None){
              if(opOperand == IROperator.NOT){
                str += s"ldc 1\n"
                str += s"iload ${right.getId()}\n"
                str += s"${irTypeToInstructionPrefix(opType)}${irOperatorToInstruction(opOperand)}\n"
                str += storeStr
              }
              else if (opOperand == IROperator.NOP){
                if(right.isDereference()){
                  if(targ.isDereference()){
                    val dTmp = targ.getDerefTemp().get
                    str += s"aload ${targ.getId()} \n"
                    str += s"iload ${dTmp.getId()} \n"
                    str += s"aload ${right.getId()} \n"
                    str += s"iload ${right.getDerefTemp().get.getId()} \n"
                    str += s"${irTypeToInstructionPrefix(right.getType())}load \n"
                    str += s"${irTypeToInstructionPrefix(targ.getType())}store \n"
                  }
                  else {
                    str += s"aload ${right.getId()} \n"
                    str += s"iload ${right.getDerefTemp().get.getId()} \n"
                    str += s"${irTypeToInstructionPrefix(right.getType())}load \n"
                    str += storeStr
                  }
                }
                else {
                  if(targ.isDereference()){
                    val dTmp = targ.getDerefTemp().get
                    str += s"aload ${targ.getId()} \n"
                    str += s"iload ${dTmp.getId()} \n"
                    str += s"${irTypeToInstructionPrefix(right.getType())}load ${right.getId()}\n"
                    str += s"${irTypeToInstructionPrefix(targ.getType())}store \n"
                  }
                  else {
                    if(isArrayType(targ.getType())){
                      str += s"aload ${right.getId()} \n"
                      str += s"astore ${targ.getId()} \n"
                    }
                    else {
                      str += s"${irTypeToInstructionPrefix(right.getType())}load ${right.getId()}\n"
                      str += storeStr
                    }
                  }
                }
              }
            }
            else if(right != None){
              val left  = exp.getLeft().get

              if(opOperand != IROperator.LESS && opOperand != IROperator.EQ){
                //non conditional
                if(left.getType() == IRType.U){
                  //string concat
                  str += "new java/lang/StringBuffer \n"
                  str += "dup \n"
                  str += "invokenonvirtual java/lang/StringBuffer/<init>()V \n"
                  str += s"aload ${left.getId()} \n"
                  str += "invokevirtual java/lang/StringBuffer/append(Ljava/lang/String;)Ljava/lang/StringBuffer; \n"
                  str += s"aload ${right.getId()} \n"
                  str += "invokevirtual java/lang/StringBuffer/append(Ljava/lang/String;)Ljava/lang/StringBuffer; \n"
                  str += "invokevirtual java/lang/StringBuffer/toString()Ljava/lang/String; \n"
                }
                else {
                  str += s"${irTypeToInstructionPrefix(left.getType())}load ${left.getId()}\n"
                  str += s"${irTypeToInstructionPrefix(right.getType())}load ${right.getId()}\n"
                  str += s"${irTypeToInstructionPrefix(opType)}${irOperatorToInstruction(opOperand)}\n"
                }

                str += storeStr
              }
              else {
                //conditional
                str += s"${irTypeToInstructionPrefix(left.getType())}load ${left.getId()}\n"
                str += s"${irTypeToInstructionPrefix(right.getType())}load ${right.getId()}\n"

                val labelTrue = s"CON_TRUE_${nextNum()}"
                val labelDone = s"CON_DONE_${nextNum()}"

                if(left.getType() == IRType.F){
                  str += "fcmpg \n"
                }
                else if (left.getType() == IRType.U){
                  str += "swap \n"
                  str += "invokevirtual java/lang/String/compareTo(Ljava/lang/String;)I \n"
                }
                else {
                  str += s"${irTypeToInstructionPrefix(left.getType())}sub\n"
                }
                str += s"i${irOperatorToInstruction(opOperand)} ${labelTrue} \n"
                str += "ldc 0\n"
                str += s"goto ${labelDone}\n"
                str += s"${labelTrue}:\n"
                str += "ldc 1 \n"
                str += s"${labelDone}:\n"
                str += storeStr
              }

            }
          }
          case i: IRInstructionBase => {
            "UNKNOWN \n"
          }
        }
      })

      str += "F_END: \n"
      str += ".end method\n"
      str
    }
    else {
      str
    }
  }

  def isArrayType(typ : IRType.Type):Boolean ={
    typ match {
      case IRType.AI => true
      case IRType.AF => true
      case IRType.AB => true
      case IRType.AC => true
      case IRType.AU => true
      case IRType.AZ => true
      case _ => false
    }
  }

  //redundent I know
  def irTypeToInstructionPrefix(typ : IRType.Type): String = {
    typ match{
      case IRType.I => "i"
      case IRType.F => "f"
      case IRType.B => "b"
      case IRType.C => "i"
      case IRType.U => "a"
      case IRType.Z => "i"
      case IRType.V => "v"
      case IRType.AI => "ia"
      case IRType.AF => "fa"
      case IRType.AB => "ba"
      case IRType.AC => "ia"
      case IRType.AU => "aa"
      case IRType.AZ => "ia"
      case IRType.ARG => "BAD"
      case IRType.BAD => "BAD"
    }
  }

  def typeToString(typ: IRType.Type): String = {
    typ match{
      case IRType.I => "I"
      case IRType.F => "F"
      case IRType.B => "B"
      case IRType.C => "I"
      case IRType.U => "Ljava/lang/String;"
      case IRType.Z => "I"
      case IRType.V => "V"
      case IRType.AI => "[I"
      case IRType.AF => "[F"
      case IRType.AB => "[B"
      case IRType.AC => "[I"
      case IRType.AU => "[Ljava/lang/String;"
      case IRType.AZ => "[I"
      case IRType.ARG => "BAD"
      case IRType.BAD => "BAD"
    }
  }

  def typeToString(typ: List[IRType.Type]): String ={
    if(typ.size > 0){
      typ.map((t) => typeToString(t)).reduce((acc,t) => acc + t)
    }
    else {
      ""
    }
  }

  def irTypeToJavaArrayType(typ : IRType.Type): String = {
    typ match{
      case IRType.AI => "int"
      case IRType.AF => "float"
      case IRType.AB => "byte"
      case IRType.AC => "int"
      case IRType.AU => throw new Exception("you must use anewarray you dumb!")
      case IRType.AZ => "boolean"
      case _ => throw new Exception("irTypeToJavaArrayType, typ is not array type!")
    }
  }

  def irOperatorToInstruction(op: IROperator.Type): String = {
    op match {
      case IROperator.ADD => "add"
      case IROperator.SUB => "sub"
      case IROperator.MUL => "mul"
      case IROperator.DIV => "div"
      case IROperator.LESS => "flt"
      case IROperator.EQ  => "feq"
      case IROperator.NOT => "xor"
      case IROperator.NOP => ""
    }
  }

  private var num: Int = 0
  def nextNum(): Int ={
    num += 1
    num
  }


  private val boilerplate = s"""
;--------------------------------------------;
;                                            ;
; Boilerplate                                ;
;                                            ;
;--------------------------------------------;
.method public static main([Ljava/lang/String;)V
	; set limits used by this method
	.limit locals 1
	.limit stack 4
	invokestatic ${className}/__main()V
	return
.end method

; standard initializer
.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method"""

}


/*
getstatic java/lang/System/out Ljava/io/PrintStream;
iload 2
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
iload 0
invokevirtual java/io/PrintStream/println(I)V
ldc "HELLO WORLD"
astore 8
getstatic java/lang/System/out Ljava/io/PrintStream;
aload 8
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
*/

/* JASMIN EXAMPLE
.source test.ir
.class public foobar
.super java/lang/Object

.method public static less(II)V
	.limit locals 4
	.var 0 is T0  I from L_0 to L_1
	.var 1 is T1  I from L_0 to L_1
	.var 2 is T2  Z from L_0 to L_1
	.var 3 is T3  Z from L_0 to L_1
	.limit stack 16
L_0:
	ldc 0
	istore 2
	ldc 0
	istore 3
.line 8
;		T2 := T0 I < T1;
	iload 0
	iload 1
	isub
	iflt L_2
	ldc 0
	goto L_3
L_2:
	ldc 1
L_3:
	istore 2
.line 9
;		T3 := Z ! T2;
	iload 2
	ldc 1
	ixor
	istore 3
.line 10
;		IF T3 GOTO L0;
	iload 3
	ifne L0
.line 11
;		PRINTI T0;
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 0
	invokevirtual java/io/PrintStream/print(I)V
.line 12
;		L0:;
L0:
.line 13
;		PRINTI T1;
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 1
	invokevirtual java/io/PrintStream/print(I)V
.line 14
;		RETURN;
	return
L_1:
.end method

.method public static __main()V
	.limit locals 2
	.var 0 is T0  I from L_4 to L_5
	.var 1 is T1  I from L_4 to L_5
	.limit stack 16
L_4:
	ldc 0
	istore 0
	ldc 0
	istore 1
.line 20
;		T0 := 2;
	ldc 2
	istore 0
.line 21
;		T1 := 3;
	ldc 3
	istore 1
.line 22
;		CALL less(T0 T1 );
	iload 0
	iload 1
	invokestatic foobar/less(II)V
.line 23
;		RETURN;
	return
L_5:
.end method

;--------------------------------------------;
;                                            ;
; Boilerplate                                ;
;                                            ;
;--------------------------------------------;

.method public static main([Ljava/lang/String;)V
	; set limits used by this method
	.limit locals 1
	.limit stack 4
	invokestatic foobar/__main()V
	return
.end method

; standard initializer
.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method
*/
