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

class JasminTests extends FlatSpec {

  "valid .ul files " should "produce jasmin code" in {
    val compManager = setup()

    var ulFiles = getULFilesInDir("src/test/scala/resource/valid/")
    ulFiles.foreach( (ulFile) => {
      println(s"running jasmin generation on: $ulFile")
      val jasminString = compManager.compile(ulFile)
      assert(jasminString.nonEmpty)
      print(jasminString.get)
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
