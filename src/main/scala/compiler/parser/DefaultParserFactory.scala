package compiler.parser

import org.antlr.runtime._

import util.SimpleFactory
import util.logging.Logger._

class DefaultParserFactory extends SimpleFactory[ulNoActionsLexer, ulNoActionsParser] {
  override def create(lexer : ulNoActionsLexer): ulNoActionsParser = {
    logMsg("creating parser",Level.INFO)
    return new ulNoActionsParser(new CommonTokenStream(lexer))
  }
}
