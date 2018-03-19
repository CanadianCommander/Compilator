package frontend

import org.antlr.runtime._

import util.logging.Logger._
import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import compiler.typechecker.DefaultTypeCheckerFactory
import compiler.CompilationManager
import frontend.ui.TreePrinter
import compiler.ast.NodeBase
import compiler.ir._

object Main {
  def main(args: Array[String]) = {
    //quick N dirty run just for now
    if(args.length == 0){
      println(s"usage:java -jar ./Compilator-all.jar <.ulfile> ")
    }
    else {
      val compManager = (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[List[IRBuilder]]]
                                    (new DefaultLexerFactory(), new DefaultParserFactory(),
                                     new DefaultTypeCheckerFactory(), new DefaultIRFactory()))
      logMsg("-compilation started-", Level.INFO)
      val ir = compManager.compile(args(0))
      if(ir.isEmpty){
        println("Compilation Failed")
        logMsg("Compilation Failed", Level.CERROR);
      }
      else{
        println("-Compilation Complete-")
        println("PROG foobar")
        ir.get.foreach((f) => print(f))
      }
      logMsg("-compilation complete-", Level.INFO)
    }

  }
}
