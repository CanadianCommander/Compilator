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

functionBody : '{' statment* '}'
             ;

compoundType : TYPE | (TYPE '[' INTEGERCONST ']')
             ;

statment     : ((expression | (PRINT_KEYWORD expression) | (RETURN_KEYWORD expression?) | (ID '=' expression) | (ID '[' expression ']' '=' expression) )? ';') |
               ((WHILE_KEYWORD '(' expression ')' block) | (IF_KEYWORD '(' expression ')' block (ELSE_KEYWORD block)?))
             ;

expression : (literal | ID | ('(' expression ')'))  (OPERATOR expression)?
           ;

block      : '{' statment* '}'
           ;

literal        : INTEGERCONST
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

OPERATOR : ('==' | '<' | '+' | '-' | '*')
         ;

ID      : ('a'..'z' | 'A'..'Z')+
        ;

INTEGERCONST : ('0'..'9')+
             ;

WS      : ( '\t' | ' ' | ('\r' | '\n') )+ { $channel = HIDDEN;}
        ;

COMMENT : '//' ~('\r' | '\n')* ('\r' | '\n') { $channel = HIDDEN;}
        ;
