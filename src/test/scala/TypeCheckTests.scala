import org.scalatest._
import org.antlr.runtime._

import util.logging.Logger._
import compiler.ast.NodeBase
import compiler.CompilationManager
import compiler.lexer.DefaultLexerFactory
import compiler.parser.DefaultParserFactory
import compiler.typechecker.DefaultTypeCheckerFactory
import util.SimpleFactory
import util.General._

class TypeCheckTests extends FlatSpec {

  "valid .ul files " should "type check correctly" in {
    val compManager = setup()

    var ulFiles = getULFilesInDir("src/test/scala/resource/valid/")
    ulFiles.foreach( (ulFile) => {
      println(s"running type check on: $ulFile")
      assert(compManager.compile(ulFile))
    });

    printBarNotification(s"${ulFiles.size} files type checked")
  }

  "invalid .ul files " should "fail to type check " in {
    val compManager = setup()

    var ulFiles = getULFilesInDir("src/test/scala/resource/invalidType/")
    ulFiles.foreach( (ulFile) => {
      println(s"running type check on: $ulFile")
      assert(!compManager.compile(ulFile))
    });

    printBarNotification(s"${ulFiles.size} files type checked")
  }

  def setup(): CompilationManager[ulNoActionsLexer,ulNoActionsParser,Boolean] = {
    return (new CompilationManager[ulNoActionsLexer,ulNoActionsParser,Boolean]
                                  (new DefaultLexerFactory(), new DefaultParserFactory(),
                                   new DefaultTypeCheckerFactory())
            )
  }
}
