grammar ulNoActions;

@header {
  package org.antlr.runtime;
  import  compiler.ast.nodes.*;
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
            : cType = compoundType id=ID {fdn.addChild(cType);fdn.addChild(new UNode($id.text));} '(' (formalParams[fParam])? ')'  {fdn.addChild(fParam);}
            ;

formalParams[FormalParamsNode topFp] returns [FormalParamsNode fpn]
              @init{
                  fpn = topFp;
              }
              : cType=compoundType ID {FunctionArgNode fArg = new FunctionArgNode(); fArg.addChild(cType); fArg.addChild(new UNode($ID.text));fpn.addChild(fArg);} (',' formalParams[fpn])?
              ;

functionBody returns [FunctionBodyNode fbn]
              @init {
                fbn = new FunctionBodyNode();
              }
              : '{' (vD=variableDec {fbn.addChild(vD);})* (( st1=statmentT1 {fbn.addChild(st1);}) | (st2=statmentT2 {fbn.addChild(st2);}))* '}'
              ;

variableDec returns [VariableDeclarationNode varDec]
            : cType=compoundType ID ';' {varDec = new VariableDeclarationNode(); varDec.addChild(cType); varDec.addChild(new UNode($ID.text));}
            ;

compoundType returns [UNode typ]
              : (TYPE {typ = new UNode($TYPE.text);}) | (TYPE '[' INTEGERCONST ']' {typ = new UNode($TYPE.text + '[' + $INTEGERCONST.text + "]");})
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

//expression: ((literal expressionHelper) | (varReference expressionHelper) | ('(' expression ')' expressionHelper) | (functionCall expressionHelper))
//          ;

//expressionHelper : (OPERATOR expression expressionHelper)?
//           ;
/*
expression      returns [ExpressionNode eNode]
                : expression2 expression02
                ;
expression02    returns [ExpressionNode eNode]
                : ((expressionAdd expression) | (expressionMult expression))?
                ;
expression2     returns [ExpressionNode eNode]
                : ((expression2 expressionMult) | (expressionEnd))
                ;

expressionEnd   returns [ExpressionNode eNode]
                : atom
                ;
// operators
expressionAdd   : OPERATOR_ADD expression2
                ;
expressionSub   : OPERATOR_SUB INTEGERCONST
                ;
expressionMult  : OPERATOR_MULT expressionEnd
                ;
expressionLess  : OPERATOR_LESS INTEGERCONST
                ;
expressionEqual : OPERATOR_EQAUL INTEGERCONST
                ;
*/
/*
expression   [boolean skipExp] returns [ExpressionNode eNode]
                @init {
                  eNode = new ExpressionNode();
                }
                :((({!skipExp}?=> expression[true]) | atom) (eMult=expressionMult | eSub=expressionSub | eAdd=expressionAdd | eLess=expressionLess | eEqual=expressionEqual)?)
                {eNode.addChild(eMult);eNode.addChild(eSub);eNode.addChild(eAdd);eNode.addChild(eLess);eNode.addChild(eEqual);}
                ;
*/
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
/*expression00      returns [ExpressionNode eNode]
                @init {
                  eNode = new ExpressionNode();
                }
                : ((eMult=expressionMult |eSub=expressionSub | eAdd=expressionAdd | eLess=expressionLess | eEqual=expressionEqual)? a1=expression)?
                ;
*/
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
                : ((vRef=varReference {aNode = vRef;})| func=functionCall {aNode=func;} | (literal {aNode = new AtomLiteralNode($literal.text);}))
                ;

varReference    returns [AtomVariableReferenceNode vrn]
                @init {
                  vrn = new AtomVariableReferenceNode();
                  IdentifierNode ident = new IdentifierNode();
                }
                : ((id1=ID {ident.setText($id1.text);}) | (id2=ID '[' exp=expression ']' {ident.setText($id2.text);}))
                {vrn.addChild(ident);vrn.addChild(exp);}
                ;

functionCall    returns [AtomFunctionCallNode funcNode]
                @init {
                  funcNode = new AtomFunctionCallNode();
                }
                : ID '(' expList=expressionList? ')'
                { funcNode.addChild(new IdentifierNode($ID.text));
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

literal         : INTEGERCONST | STRING_CONST | FLOAT_CONST | CHAR_CONST | TRUE_CONST | FALSE_CONST
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
