grammar ulNoActions;

@header {
  package org.antlr.runtime;
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
}

@rulecatch {
        catch (RecognitionException ex) {
                reportError(ex);
                throw ex;
        }
}

program : function+
	      ;

function : functionDecl functionBody
         ;

functionDecl : compoundType ID '(' formalParams? ')'
             ;

formalParams : compoundType ID (',' formalParams)?
             ;

functionBody : '{' variableDec* (statmentT1 | statmentT2 | statmentT3)* '}'
             ;

variableDec : compoundType ID ';'
            ;

compoundType : TYPE | (TYPE '[' INTEGERCONST ']')
             ;

statmentT1 : ((PRINT_KEYWORD expression) | (RETURN_KEYWORD expression?) | (varReference '=' expression)) ';'
           ;

statmentT2 : ((WHILE_KEYWORD '(' expression ')' block) | (IF_KEYWORD '(' expression ')' block (ELSE_KEYWORD block)?))
           ;

statmentT3 : functionCall ';'
           ;

expression: ('(' expressionHelper ')' (OPERATOR expression)?) | expressionHelper
          ;

expressionHelper : (literal | varReference | functionCall)  (OPERATOR expression)?
           ;

expressionList : expression (',' expression)?
               ;

varReference: ID | (ID '[' expression ']')
            ;

functionCall: ID '(' expressionList? ')'
            ;

block      : '{' (statmentT1 | statmentT2 | statmentT3)* '}'
           ;

literal        : INTEGERCONST | STRING_CONST | FLOAT_CONST | CHAR_CONST | TRUE_CONST | FALSE_CONST
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

OPERATOR : ('==' |'<' | '+' | '-' | '*')
         ;

ID      : ('a'..'z' | 'A'..'Z' | '_')('a'..'z' | 'A'..'Z' | '_' | '0'..'9')*
        ;

INTEGERCONST : ('0'..'9')+
             ;

STRING_CONST : '"' (~'"')* '"'
             ;

FLOAT_CONST  : ('0'..'9')*'.'('0'..'9')+
             ;

CHAR_CONST   : '\''('a'..'z' | 'A'..'Z')'\''
             ;

TRUE_CONST   : 'true';
FALSE_CONST  : 'false';

WS      : ( '\t' | ' ' | ('\r' | '\n') )+ { $channel = HIDDEN;}
        ;

COMMENT : '//' ~('\r' | '\n')* ('\r' | '\n') { $channel = HIDDEN;}
        ;
