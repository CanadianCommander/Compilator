package compiler
import java.io.{FileInputStream}

import util.logging.Logger._
import util.General._
import util.SimpleFactory

// coordinates the compilation process.
// args:
//  lexerFactory : factory function that constructs the lexer
//  parserFactory: factory function that constructs the parser
class CompilationManager[A,B,C](lexerFactory: SimpleFactory[FileInputStream,A], parserFactory: SimpleFactory[A,B],
                              typecheckerFactory : SimpleFactory[B,C]) {

  def compile(targetFile: String): C = {
    try{
      val out = typecheckerFac.create(
                parserFac.create(
                lexerFac.create(getInputStream(targetFile)))
      )
      return out
    }
    catch{
      case e : Exception => {
        logMsg(s"Unrecoverable error: $e", Level.CERROR)
        return null.asInstanceOf[C]
      }
    }

    return null.asInstanceOf[C]
  }


  val lexerFac = lexerFactory
  val parserFac = parserFactory
  val typecheckerFac = typecheckerFactory
}
