import org.scalatest._
import org.antlr.runtime._

import util.logging.Logger._
import compiler.CompilationManager

import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import util.SimpleFactory

class LexerTest extends FlatSpec {
  "valid .ul files" should "pass lexer and parser" in {
    val compManager = setup()

    val prefix = "src/test/scala/resource/valid/"
    assert(compManager.compile(s"${prefix}/accept1.ul"))

  }

  "invalid .ul files" should "fail parser or lexer" in {
    val compManager = setup()

    val prefix = "src/test/scala/resource/invalid/"
    assert(!compManager.compile(s"${prefix}/reject1.ul"))
  }

  def setup(): CompilationManager[ulNoActionsLexer,ulNoActionsParser,Boolean] = {
    closeLog()
    setLogFilePath("compilator.log")
    return (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Boolean]
                                  (new DefaultLexerFactory(), new DefaultParserFactory(),
                                   new lexerTestFactory())
            )
  }
}

//testing compilation end point (runs only lexer and parser). returning true on success, false otherwise
class lexerTestFactory extends SimpleFactory[ulNoActionsParser, Boolean] {
  override def create(parser: ulNoActionsParser): Boolean = {
    try{
      parser.program()
    }
    catch{
      case e : RecognitionException => {
        logMsg(s"Parsing error: $e", Level.CERROR)
        return false
      }
    }

    return true
  }
}
