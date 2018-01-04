grammar ulNoActions;
				
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

program : IF NEWLINE
	;


/* Lexer */
	 
IF	: 'if'
	;

/* NB: This will not be part of your grammar.  Whitespace should be ignored.  
       This is only here so that you have a complete example 
 */
NEWLINE: '\n'
	;
