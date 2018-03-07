package frontend

import org.antlr.runtime._

import util.logging.Logger._
import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import compiler.typechecker.DefaultTypeCheckerFactory
import compiler.CompilationManager
import frontend.ui.TreePrinter
import compiler.ast.NodeBase

object Main {
  def main(args: Array[String]) = {
    //quick N dirty run just for now
    if(args.length == 0){
      println(s"usage:java -jar ./Compilator-all.jar <.ulfile> ")
    }
    else {
      val compManager = (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[NodeBase]]
                            (new DefaultLexerFactory(), new DefaultParserFactory(),new DefaultTypeCheckerFactory()))
      logMsg("-compilation started-", Level.INFO)
      if(compManager.compile(args(0)).isEmpty){
        println("Compilation Failed")
        logMsg("Compilation Failed", Level.CERROR);
      }
      else{
        println("Compilation Complete")
      }
      logMsg("-compilation complete-", Level.INFO)
    }

  }
}
