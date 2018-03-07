package compiler.ir

import util.logging.Logger._

class IRException (msg: String) extends Exception {
  val myMsg = msg
  logMsg(getMessage(),Level.ERROR)//code in class def! my god The POWER!

  override def getMessage(): String = {
    s"Error:${myMsg}"
  }
}
