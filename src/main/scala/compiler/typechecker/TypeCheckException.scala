package compiler.typechecker

class TypeCheckException (msg: String, line: Int) extends Exception {
  val myMsg = msg
  val myLine = line

  override def getMessage(): String = {
    s"Error:${myLine}:${myMsg}"
  }
}
