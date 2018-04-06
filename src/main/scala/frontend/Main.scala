package frontend

import org.antlr.runtime._

import util.logging.Logger._
import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import compiler.jasmin.DefaultJasminFactory
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
      val compManager = (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[List[IRBuilder]], Option[String]]
                                    (new DefaultLexerFactory(), new DefaultParserFactory(),
                                     new DefaultTypeCheckerFactory(), new DefaultIRFactory(), new DefaultJasminFactory(getBaseName(args(0)))))
      logMsg("-compilation started-", Level.INFO)
      val jasmin = compManager.compile(args(0))
      if(jasmin == null || jasmin.isEmpty){
        println("Compilation Failed see log for details")
        logMsg("Compilation Failed", Level.CERROR);
      }
      else{
        logMsg("-Compilation Complete-", Level.INFO)
        print(jasmin.get)
      }
      logMsg("-compilation complete-", Level.INFO)
    }

  }

  // come on java, no out of the box base name function! plz
  def getBaseName(str: String): String ={
    val bname = raw"(.*/)?([\d\w\W]+)\.[\W\w\d]+".r

    str match{
      case bname(_, s) => s
      case _ => "NAME_ERROR"
    }
  }
}
