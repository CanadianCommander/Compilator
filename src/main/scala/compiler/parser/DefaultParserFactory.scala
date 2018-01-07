package compiler.parser

import org.antlr.runtime._

import util.SimpleFactory

class DefaultParserFactory extends SimpleFactory[ulNoActionsLexer, ulNoActionsParser] {
  override def create(lexer : ulNoActionsLexer): ulNoActionsParser = {
    return new ulNoActionsParser(new CommonTokenStream(lexer))
  }
}
