import org.scalatest._
import org.antlr.runtime._

import util.logging.Logger._
import compiler.ast.NodeBase
import compiler.CompilationManager
import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import compiler.typechecker.DefaultTypeCheckerFactory
import compiler.ir._
import util.SimpleFactory
import util.General._

class IRTests extends FlatSpec {

  "valid .ul files " should "produce IR tree" in {
    val compManager = setup()

    var ulFiles = getULFilesInDir("src/test/scala/resource/valid/")
    ulFiles.foreach( (ulFile) => {
      println(s"running IR generation on: $ulFile")
      val res =compManager.compile(ulFile)
      assert(res.nonEmpty)
      res.get.foreach((b) => print(b))
    });

    printBarNotification(s"${ulFiles.size} files type checked")
  }


  def setup(): CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[List[IRBuilder]], Option[List[IRBuilder]]] = {
    return (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[List[IRBuilder]], Option[List[IRBuilder]]]
                                  (new DefaultLexerFactory(), new DefaultParserFactory(),
                                   new DefaultTypeCheckerFactory(), new DefaultIRFactory())
            )
  }
}
