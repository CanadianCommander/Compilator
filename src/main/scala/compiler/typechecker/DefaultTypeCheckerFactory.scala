package compiler.typechecker

import org.antlr.runtime._

import util.SimpleFactory
import compiler.ast.NodeBase
import util.logging.Logger._
import util.General._
import frontend.ui.TreePrinter

//TODO replace output string with parser object of some sort !!!
class DefaultTypeCheckerFactory extends SimpleFactory[ulNoActionsParser, TreePrinter] {
  override def create(parser: ulNoActionsParser): TreePrinter = {
    try{
      //tmp, until type checking implemented
      logMsg("invoking antlr parser and lexer", Level.INFO)
      var root: NodeBase = parser.program()

      logMsg("refactoring AST", Level.INFO)
      cleanAST(root)
      return new TreePrinter(root)
    }
    catch{
      case e : RecognitionException => {
        logMsg(s"Parsing error: $e", Level.CERROR)
      }
    }
    return null;
  }
}
