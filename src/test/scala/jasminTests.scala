import org.scalatest._
import org.antlr.runtime._

import util.logging.Logger._
import compiler.ast.NodeBase
import compiler.CompilationManager
import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import compiler.typechecker.DefaultTypeCheckerFactory
import compiler.jasmin.DefaultJasminFactory
import compiler.ir._
import util.SimpleFactory
import util.General._
import sys.process._
import java.io.{PrintWriter, BufferedWriter, FileWriter, IOException, File}

class JasminTests extends FlatSpec {

  "valid .ul files " should "produce jasmin code" in {
    val compManager = setup()

    var ulFiles = getULFilesInDir("src/test/scala/resource/valid/")
    ulFiles.foreach( (ulFile) => {
      println(s"running jasmin generation on: $ulFile")
      val jasminString = compManager.compile(ulFile)
      assert(jasminString.nonEmpty)

      //run the code!
      val tFile = new File("tmp.j")
      if(tFile.isFile){
        tFile.delete()
      }
      val jFile = new File("foobar.class")
      if(jFile.isFile){
        jFile.delete()
      }
      var outStream = new PrintWriter(new BufferedWriter( new FileWriter(tFile)))
      outStream.print(jasminString.get)
      outStream.flush()
      outStream = null


      var exitCode = "java -jar 3rdParty/jasmin-2.4/jasmin.jar tmp.j".!
      assert(exitCode == 0)
      exitCode = "java foobar".!
      assert(exitCode == 0)
    });

    printBarNotification(s"${ulFiles.size} files type checked")
  }


  def setup(): CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[List[IRBuilder]], Option[String]] = {
    return (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[List[IRBuilder]], Option[String] ]
                                  (new DefaultLexerFactory(), new DefaultParserFactory(),
                                   new DefaultTypeCheckerFactory(), new DefaultIRFactory(), new DefaultJasminFactory("foobar"))
            )
  }
}
