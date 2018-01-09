import org.scalatest._
import scala.collection.JavaConversions._
import org.antlr.runtime._
import java.io.File

import util.logging.Logger._
import compiler.CompilationManager

import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import util.SimpleFactory

class LexerTest extends FlatSpec {
  "valid .ul files" should "pass lexer and parser" in {
    val compManager = setup()

    val ulFiles = getULFilesInDir("src/test/scala/resource/valid/")
    ulFiles.foreach((f) => {
      println(s"testing: $f")
      assert(compManager.compile(f))
    })
    println(s"\n////////////// ${ulFiles.size} files tested for accept /////////////////////\n");
  }

  "invalid .ul files" should "fail parser or lexer" in {
    val compManager = setup()

    val ulFiles = getULFilesInDir("src/test/scala/resource/invalid/")
    ulFiles.foreach((f) => {
      println(s"testing: $f")
      assert(!compManager.compile(f))
    })
    println(s"\n////////////// ${ulFiles.size} files tested for reject /////////////////////\n");
  }

  def getULFilesInDir(dir: String): List[String] = {
    val dirFd = new File(dir)

    if(dirFd.isDirectory){
      return (dirFd.listFiles()).toList.filter((f) => {
                                          var ext = (f.getAbsolutePath.split("\\.(?=[\\w\\d]+$)"))
                                          if(ext.size < 2){
                                            false
                                          }
                                          else{
                                            ext(1).compare("ul") == 0
                                          }
                                        })
                                        .map((f) => f.getAbsolutePath())
    }
    return null
  }

  def setup(): CompilationManager[ulNoActionsLexer,ulNoActionsParser,Boolean] = {
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
