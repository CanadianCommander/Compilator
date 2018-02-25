grammar ulNoActions;

@header {
  package org.antlr.runtime;
  import  compiler.ast.nodes.*;
  import  compiler.ast.ASTType;
  import  compiler.ast.NodeBase;
}

@lexer::header{
  package org.antlr.runtime;
}

@members
{
  protected void mismatch (IntStream input, int ttype, BitSet follow)
          throws RecognitionException
  {
          throw new MismatchedTokenException(ttype, input);
  }
  public void recoverFromMismatchedSet (IntStream input,
                                        RecognitionException e,
                                        BitSet follow)
          throws RecognitionException
  {
          reportError(e);
          throw e;
  }
  private UNode createUNode(NodeBase parent){
    UNode node = new UNode();
    parent.addChild(node);
    return node;
  }
}

@rulecatch {
        catch (RecognitionException ex) {
                reportError(ex);
                throw ex;
        }
}

program returns [RootNode r]
        @init {
          r = new RootNode();
        }
        : (f=function {r.addChild(f);})+
	      ;

function returns [FunctionNode f]: fd=functionDecl fb=functionBody
                                   {f=new FunctionNode(fd,fb);}
                                 ;

functionDecl returns [FunctionDeclarationNode fdn]
            @init{
              fdn = new FunctionDeclarationNode();
              FormalParamsNode fParam = new FormalParamsNode();
            }
            : cType = compoundType id=ID {fdn.addChild(cType);fdn.addChild(new IdentifierNode($id.text));} '(' (formalParams[fParam])? ')'  {fdn.addChild(fParam);}
            ;

formalParams[FormalParamsNode topFp] returns [FormalParamsNode fpn]
              @init{
                  fpn = topFp;
              }
              : cType=compoundType ID {FunctionArgNode fArg = new FunctionArgNode(); fArg.addChild(cType);fArg.setLineNumber($ID.getLine()); fArg.addChild(new IdentifierNode($ID.text));fpn.addChild(fArg);} (',' formalParams[fpn])?
              ;

functionBody returns [FunctionBodyNode fbn]
              @init {
                fbn = new FunctionBodyNode();
              }
              : '{' (vD=variableDec {fbn.addChild(vD);})* (( st1=statmentT1 {fbn.addChild(st1);}) | (st2=statmentT2 {fbn.addChild(st2);}))* '}'
              ;

variableDec returns [VariableDeclarationNode varDec]
            : cType=compoundType ID ';' {varDec = new VariableDeclarationNode(); varDec.addChild(cType); varDec.setLineNumber($ID.getLine()); varDec.addChild(new IdentifierNode($ID.text));}
            ;

compoundType returns [CompTypeNode typ]
              : (TYPE {typ = new CompTypeNode($TYPE.text);typ.setType(ASTType.javaCompat($TYPE.text));})
              | (TYPE '[' INTEGERCONST ']' {typ = new CompTypeNode($TYPE.text); typ.setType(ASTType.javaCompat("A" + $TYPE.text));typ.setLenS($INTEGERCONST.text);})
              ;

statmentT1 returns [StatementNode sn]
           :  ((({sn = new PrintStatementNode();} PRINT_KEYWORD expP=expression {sn.addChild(expP);sn.setText($PRINT_KEYWORD.text);})
                | ({sn = new AssignStatementNode();} target=expression ('=' exp2=expression)? {sn.addChild(target);sn.addChild(exp2);}))?
              ';')
              | ({sn = new ReturnStatementNode();} RETURN_KEYWORD (exp=expression)? {sn.addChild(exp);}';')
           ;

statmentT2 returns [StatementNode sn]
           : ((WHILE_KEYWORD '(' exp1=expression  ')' b1=block {sn = new WhileStatementNode();sn.addChild(exp1);sn.addChild(b1);})
             | (IF_KEYWORD '(' exp2=expression ')' b2=block
              {sn = new StatementNode(); IfStatementNode ifSt = new IfStatementNode();ifSt.addChild(exp2);ifSt.addChild(b2);sn.addChild(ifSt);}
              (ELSE_KEYWORD b3=block {ElseStatementNode elseSt = new ElseStatementNode();elseSt.addChild(b3);sn.addChild(elseSt);})?))
           ;

expression      returns [ExpressionNode eNode]
                @init {
                  eNode = new ExpressionNode();
                }
                : expFirst=expression0 {eNode.addChild(expFirst);} work[eNode]
                ;
work            [ExpressionNode eNode]
                : ((eEq=expressionEqual {eNode.addChild(eEq);}) (work[eNode])?)?
                ;

expression0     returns [ExpressionNode eNode]
                @init {
                  eNode = new ExpressionNode();
                }
                : expFirst=expression1 {eNode.addChild(expFirst);} work0[eNode]
                ;
work0           [ExpressionNode eNode]
                : ((eLess=expressionLess {eNode.addChild(eLess);}) (work0[eNode])?)?
                ;

expression1     returns [ExpressionNode eNode]
                @init {
                  eNode = new ExpressionNode();
                }
                : expFirst=expression2 {eNode.addChild(expFirst);} work1[eNode]
                ;
work1           [ExpressionNode eNode]
                : ((opSub=expressionSub | opAdd=expressionAdd) {eNode.addChild(opSub);eNode.addChild(opAdd);} (work1[eNode])?)?
                ;

expression2     returns [ExpressionNode eNode]
                @init {
                  eNode = new ExpressionNode();
                }
                : expFirst=expressionEnd {eNode.addChild(expFirst);} work2[eNode]
                ;

work2           [ExpressionNode eNode]
                : ((opMult=expressionMult) {eNode.addChild(opMult);} (work2[eNode])?)?
                ;

expressionEnd   returns [ExpressionNode eNode]
                @init {
                  eNode = new ExpressionNode();
                }
                : (at=atom | ('(' exp=expression ')')) {eNode.addChild(at);eNode.addChild(exp);}
                ;


expressionAdd   returns [OperationAddNode oAdd]
                : {oAdd = new OperationAddNode();} OPERATOR_ADD a2=expression2 {oAdd.addChild(a2);}
                ;
expressionSub   returns [OperationSubNode oSub]
                : {oSub = new OperationSubNode();} OPERATOR_SUB a2=expression2 {oSub.addChild(a2);}
                ;
expressionMult  returns [OperationMultNode oMult]
                : {oMult = new OperationMultNode();} OPERATOR_MULT anext=expressionEnd {oMult.addChild(anext);}
                ;
expressionLess  returns [OperationLessNode oLess]
                : {oLess = new OperationLessNode();} OPERATOR_LESS a2=expression1 {oLess.addChild(a2);}
                ;
expressionEqual returns [OperationEqualNode oEqual]
                : {oEqual = new OperationEqualNode();} OPERATOR_EQAUL a2=expression0 {oEqual.addChild(a2);}
                ;

expressionList  returns [FunctionCallArgumentsNode fArgs]
                @init{
                  fArgs = new FunctionCallArgumentsNode();
                }
                : firstExp=expression {fArgs.addChild(firstExp);} (',' expNext=expression {fArgs.addChild(expNext);})*
                ;

atom            returns [AtomNode aNode]
                : ((vRef=varReference {aNode = vRef;})| func=functionCall {aNode=func;} | (lit=literal {aNode = lit;}))
                ;

varReference    returns [AtomVariableReferenceNode vrn]
                @init {
                  vrn = new AtomVariableReferenceNode();
                  IdentifierNode ident = new IdentifierNode();
                }
                : ((id1=ID {ident.setText($id1.text);ident.setLineNumber($id1.getLine());})
                | (id2=ID '[' exp=expression ']' {ident.setText($id2.text);ident.setLineNumber($id2.getLine());vrn.setDereference();}))
                {vrn.addChild(ident);vrn.addChild(exp);vrn.setLineNumber(ident.getLineNumber());}
                ;

functionCall    returns [AtomFunctionCallNode funcNode]
                @init {
                  funcNode = new AtomFunctionCallNode();
                }
                : ID '(' expList=expressionList? ')'
                { funcNode.addChild(new IdentifierNode($ID.text));
                  funcNode.setLineNumber($ID.getLine());
                  if(expList != null){
                    funcNode.addChild(expList);
                  }
                  else{
                    funcNode.addChild(new FunctionCallArgumentsNode());
                  }
                }
                ;

block           returns [BlockNode bn]
                @init{
                  bn = new BlockNode();
                }
                : '{' ((st1=statmentT1 {bn.addChild(st1);}) | (st2=statmentT2 {bn.addChild(st2);}) )* '}'
                ;

literal         returns [AtomLiteralNode aln]
                : ((INTEGERCONST {aln = new AtomLiteralNode($INTEGERCONST.text); aln.setLineNumber($INTEGERCONST.getLine()); aln.setType(ASTType.javaCompat("Int"));})
                  | (STRING_CONST {aln = new AtomLiteralNode($STRING_CONST.text); aln.setLineNumber($STRING_CONST.getLine()); aln.setType(ASTType.javaCompat("String"));})
                  | (FLOAT_CONST {aln = new AtomLiteralNode($FLOAT_CONST.text); aln.setLineNumber($FLOAT_CONST.getLine()); aln.setType(ASTType.javaCompat("Float"));})
                  | (CHAR_CONST {aln = new AtomLiteralNode($CHAR_CONST.text); aln.setLineNumber($CHAR_CONST.getLine()); aln.setType(ASTType.javaCompat("Char"));})
                  | (TRUE_CONST {aln = new AtomLiteralNode($TRUE_CONST.text); aln.setLineNumber($TRUE_CONST.getLine()); aln.setType(ASTType.javaCompat("Bool"));})
                  | (FALSE_CONST {aln = new AtomLiteralNode($FALSE_CONST.text); aln.setLineNumber($FALSE_CONST.getLine()); aln.setType(ASTType.javaCompat("Bool"));}))
                ;

/* Lexer */

PRINT_KEYWORD : ('print' | 'println')
              ;

RETURN_KEYWORD: 'return'
              ;

ELSE_KEYWORD  :'else'
              ;

IF_KEYWORD    :'if'
              ;

WHILE_KEYWORD :'while'
              ;

TYPE    : ('int' | 'float' | 'char' | 'string' | 'boolean' | 'void')
        ;

OPERATOR_ADD  : '+'
              ;
OPERATOR_MULT : '*'
              ;
OPERATOR_LESS : '<'
              ;
OPERATOR_SUB  : '-'
              ;
OPERATOR_EQAUL: '=='
              ;

INTEGERCONST : ('0'..'9')+
             ;

STRING_CONST : '"' (~'"')* '"'
             ;

FLOAT_CONST  : ('0'..'9')*'.'('0'..'9')+
             ;

CHAR_CONST   : '\''('a'..'z' | 'A'..'Z')'\''
             ;

TRUE_CONST  : 'true'
            ;
FALSE_CONST : 'false'
            ;

ID          : ('a'..'z' | 'A'..'Z' | '_')('a'..'z' | 'A'..'Z' | '_' | '0'..'9')*
            ;

WS      : ( '\t' | ' ' | ('\r' | '\n') )+ { $channel = HIDDEN;}
        ;

COMMENT : '//' ~('\r' | '\n')* ('\r' | '\n') { $channel = HIDDEN;}
        ;
