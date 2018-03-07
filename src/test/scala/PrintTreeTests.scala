import org.scalatest._
import org.antlr.runtime._
import java.io.{InputStreamReader,BufferedReader,InputStream, PrintStream, PipedOutputStream, PipedInputStream}

import util.logging.Logger._
import compiler.ast.NodeBase
import compiler.CompilationManager
import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import util.SimpleFactory
import util.General._
import frontend.ui.TreePrinter

class PrintTreeTests extends FlatSpec {

  "tree of .ul files " should "print correctly" in {
    val compManager = setup()

    var ulFiles = getULFilesInDir("src/test/scala/resource/valid/")
    ulFiles.foreach( (ulFile) => {
      println(s"running TreePrinter on: $ulFile")
      //print parse trees of files
      val treePrinter = compManager.compile(ulFile)
      if(treePrinter != null){
        //init pipes to direct output to this testing function instead of stdout (the default for TreePrinter)
        val inputPipe = new PipedInputStream()
        val inputStream = new BufferedReader(new InputStreamReader(inputPipe))
        val outputStream = new PrintStream(new PipedOutputStream(inputPipe))

        treePrinter.setOutputStream(outputStream)
        treePrinter.printTree()

        assert(inputStream.ready())
        println("\n-------------------------------------------")
        while(inputStream.ready()){
          print(inputStream.read().asInstanceOf[Char])
        }
        println("\n-------------------------------------------\n")
      }
      else{
        assert(false)
      }
    });
    printBarNotification(s"${ulFiles.size} files testet for PrintTree")
  }

  def setup(): CompilationManager[ulNoActionsLexer,ulNoActionsParser,TreePrinter,TreePrinter] = {
    return (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,TreePrinter,TreePrinter]
                                  (new DefaultLexerFactory(), new DefaultParserFactory(),
                                   new TreePrinterTestFactory())
            )
  }
}


//testing compilation end point. It runs only the lexer and parser then returns a tree printer
class TreePrinterTestFactory extends SimpleFactory[ulNoActionsParser, TreePrinter] {
  override def create(parser: ulNoActionsParser): TreePrinter = {
    try{
      var root: NodeBase = parser.program()
      cleanAST(root)
      return new TreePrinter(root)
    }
    catch{
      case e : RecognitionException => {
        logMsg(s"Parsing error: $e", Level.CERROR)
        return null
      }
    }

    return null
  }
}
