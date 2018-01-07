package util

import java.io.FileInputStream

trait SimpleFactory[A,B] {
  def create(arg: A): B
}
