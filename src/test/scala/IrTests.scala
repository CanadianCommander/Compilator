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
      println(s"running type check on: $ulFile")
      assert(compManager.compile(ulFile).nonEmpty)
    });

    printBarNotification(s"${ulFiles.size} files type checked")
  }


  def setup(): CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[IRNodeBase]] = {
    return (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Option[NodeBase],Option[IRNodeBase]]
                                  (new DefaultLexerFactory(), new DefaultParserFactory(),
                                   new DefaultTypeCheckerFactory(), new DefaultIRFactory())
            )
  }
}
