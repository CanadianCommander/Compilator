package compiler.typechecker

import org.antlr.runtime._

import util.SimpleFactory
import util.logging.Logger._

//TODO replace output string with parser object of some sort !!!
class DefaultTypeCheckerFactory extends SimpleFactory[ulNoActionsParser, String] {
  override def create(parser: ulNoActionsParser): String = {
    try{
      parser.program()
    }
    catch{
      case e : RecognitionException => {
        logMsg(s"Parsing error: $e", Level.CERROR)
      }
    }

    return "FOO BAR, REPLACE ME!"
  }
}
