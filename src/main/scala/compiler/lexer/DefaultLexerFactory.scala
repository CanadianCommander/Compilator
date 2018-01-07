package compiler.lexer

import java.io.FileInputStream
import org.antlr.runtime._

import util.SimpleFactory

class DefaultLexerFactory extends SimpleFactory[FileInputStream, ulNoActionsLexer]{
  override def create(inStream: FileInputStream): ulNoActionsLexer = {
    return new ulNoActionsLexer(new ANTLRInputStream(inStream))
  }
}
