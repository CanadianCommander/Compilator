package util


class SimpleFactoryNop [A,B] extends SimpleFactory[A,B] {
  def create(arg: A): B ={
    arg.asInstanceOf[B]
  }
}
