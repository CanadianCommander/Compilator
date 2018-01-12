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
        : (
          {FunctionNode fNode = new FunctionNode();}
          function[fNode]
          {fNode.setText($function.text);r.addChild(fNode);}
          )+

	      ;

function[FunctionNode fNode]
        @init {
          UNode fDec = new UNode();
          FunctionBodyNode fBody = new FunctionBodyNode();
        }
        : functionDecl[fDec] functionBody[fBody]
        {
          fDec.setText($functionDecl.text);
          fBody.setText($functionBody.text);
          fNode.addChild(fDec);
          fNode.addChild(fBody);
        }
        ;

functionDecl[UNode fDec]
            @init {
              UNode cType = new UNode();
              UNode id = new UNode();
              UNode fParam = new UNode();
            }
            :
            compoundType {cType.setText($compoundType.text); fDec.addChild(cType);}
            ID {id.setText($ID.text); fDec.addChild(id);}
            '(' (formalParams {fParam.setText($formalParams.text); fDec.addChild(fParam);})? ')'
            ;

formalParams : compoundType ID (',' formalParams)?
             ;

functionBody[FunctionBodyNode fBody]
              : '{' ({UNode vDec = createUNode(fBody);} variableDec[vDec] {vDec.setText($variableDec.text);})*
                (({UNode st1 = createUNode(fBody);} statmentT1[st1] {st1.setText($statmentT1.text);})
                | ({UNode st2 = createUNode(fBody);} statmentT2[st2] {st2.setText($statmentT2.text);}) )* '}'
              ;

variableDec[UNode vDec] : compoundType ID ';'
            ;

compoundType : TYPE | (TYPE '[' INTEGERCONST ']')
             ;

statmentT1[UNode st1]
           :  ((
                ({UNode exp = createUNode(st1);} PRINT_KEYWORD expP=expression[exp] {exp.setText($expP.text);})
                | ({UNode assign = createUNode(st1); UNode target = createUNode(assign);}
                  exp1=expression[target] {target.setText($exp1.text);} ({UNode source = createUNode(assign);} '=' exp2=expression[source] {source.setText($exp2.text);})?)
              )? ';')
              | ({UNode ret = createUNode(st1);ret.setText("return");} RETURN_KEYWORD
                ({UNode exp = createUNode(ret);} expR=expression[exp] {exp.setText($expR.text);})? ';')
           ;

statmentT2[UNode st2] : ((WHILE_KEYWORD '(' expression[new UNode()] ')' block) | (IF_KEYWORD '(' expression[new UNode()] ')' block (ELSE_KEYWORD block)?))
           ;

//expression: ((literal expressionHelper) | (varReference expressionHelper) | ('(' expression ')' expressionHelper) | (functionCall expressionHelper))
//          ;

//expressionHelper : (OPERATOR expression expressionHelper)?
//           ;

// operators
/*
operation       : (expressionEqual | expressionLess | expressionAdd | expressionSub | expressionMult)
                ;
expressionAdd   : OPERATOR_ADD expression
                ;
expressionSub   : OPERATOR_SUB expression
                ;
expressionMult  : OPERATOR_MULT expression
                ;
expressionLess  : OPERATOR_LESS expression
                ;
expressionEqual : OPERATOR_EQAUL expression
                ;
*/

expression[UNode exp]      : expressionMult (expressionSub | expressionAdd | expressionLess | expressionEqual)?
                ;
expressionAdd   : OPERATOR_ADD expression[new UNode()]
                ;
expressionSub   : OPERATOR_SUB expression[new UNode()]
                ;
expressionMult  : atom (OPERATOR_MULT atom)*
                ;
expressionLess  : OPERATOR_LESS expression[new UNode()]
                ;
expressionEqual : OPERATOR_EQAUL expression[new UNode()]
                ;

expressionList  : expression[new UNode()] (',' expression[new UNode()])*
                ;

atom            : (varReference | functionCall | literal | bracketExpression)
                ;

varReference    : ID | (ID '[' expression[new UNode()] ']')
                ;

bracketExpression : '('expression[new UNode()]')'
                  ;

functionCall    : ID '(' expressionList? ')'
                ;
// TODO pass real node
block           : '{' (statmentT1[new UNode()] | statmentT2[new UNode()] )* '}'
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
