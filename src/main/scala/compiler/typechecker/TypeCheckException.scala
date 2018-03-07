package compiler.typechecker

import util.logging.Logger._

class TypeCheckException (msg: String, line: Int) extends Exception {
  val myMsg = msg
  val myLine = line
  logMsg(getMessage(),Level.ERROR)

  override def getMessage(): String = {
    s"Error:${myLine}:${myMsg}"
  }
}
