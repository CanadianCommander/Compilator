package util

import java.io.{FileInputStream, FileOutputStream, FileNotFoundException}
import java.lang.{SecurityException}

import util.logging.Logger._

//general util functions
object General {

  def getInputStream(filePath: String): FileInputStream = {
    return getStream[FileInputStream]((p) => new FileInputStream(p), filePath)
  }

  def getOutputStream(filePath: String): FileOutputStream = {
    return getStream[FileOutputStream]((p) => new FileOutputStream(p), filePath)
  }

  //open a stream on file at filePath
  def getStream[A](streamGenFunc: (String) => A, filePath: String):A = {
    try{
      return streamGenFunc(filePath);
    }
    catch{
      case e : FileNotFoundException => {
        logMsg(s"no such file: $filePath", Level.ERROR)
        throw e
      }
      case e : SecurityException => {
        logMsg(s"you have insufficient privileges to access file: $filePath", Level.ERROR)
        throw e
      }
    }
  }

}
