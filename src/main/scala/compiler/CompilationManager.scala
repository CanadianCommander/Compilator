package compiler
import java.io.{FileInputStream}

import util.logging.Logger._
import util.General._
import util.SimpleFactory
import util.SimpleFactoryNop

// coordinates the compilation process.
// args:
//  lexerFactory : factory function that constructs the lexer
//  parserFactory: factory function that constructs the parser
class CompilationManager[A,B,C,D,F](lexerFactory: SimpleFactory[FileInputStream,A], parserFactory: SimpleFactory[A,B] = new SimpleFactoryNop[A,B](),
                              typecheckerFactory : SimpleFactory[B,C] = new SimpleFactoryNop[B,C](), irFactory: SimpleFactory[C,D] = new SimpleFactoryNop[C,D](),
                              jasminFactory: SimpleFactory[D,F] = new SimpleFactoryNop[D,F]()) {

  def compile(targetFile: String): F = {
    try{
      val out = jasminFactory.create(
                irFac.create(
                typecheckerFac.create(
                parserFac.create(
                lexerFac.create(getInputStream(targetFile))))))
      return out
    }
    catch{
      case e : Exception => {
        logMsg(s"Unrecoverable error: $e", Level.CERROR)
        logMsg(s"STACK Trace:\n${e.getStackTrace().foldLeft("")((str,x) => str + x + "\n")}", Level.INFO)
        return null.asInstanceOf[F]
      }
    }

    return null.asInstanceOf[F]
  }


  val lexerFac = lexerFactory
  val parserFac = parserFactory
  val typecheckerFac = typecheckerFactory
  val irFac = irFactory
}
