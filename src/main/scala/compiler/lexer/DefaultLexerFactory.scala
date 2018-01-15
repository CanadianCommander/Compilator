package compiler.lexer

import java.io.FileInputStream
import org.antlr.runtime._

import util.SimpleFactory
import util.logging.Logger._

class DefaultLexerFactory extends SimpleFactory[FileInputStream, ulNoActionsLexer]{
  override def create(inStream: FileInputStream): ulNoActionsLexer = {
    logMsg("creating lexer",Level.INFO)
    return new ulNoActionsLexer(new ANTLRInputStream(inStream))
  }
}
