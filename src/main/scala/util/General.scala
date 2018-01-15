package util

import java.io.{FileInputStream, FileOutputStream, FileNotFoundException, File, PrintStream}
import java.lang.{SecurityException}

import util.logging.Logger._
import compiler.ast.NodeBase
import compiler.ast.nodes._

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

  def getULFilesInDir(dir: String): List[String] = {
    val dirFd = new File(dir)

    if(dirFd.isDirectory){
      return (dirFd.listFiles()).toList.filter((f) => {
                                          var ext = (f.getAbsolutePath.split("\\.(?=[\\w\\d]+$)"))
                                          if(ext.size < 2){
                                            false
                                          }
                                          else{
                                            ext(1).compare("ul") == 0
                                          }
                                        })
                                        .map((f) => f.getAbsolutePath())
    }
    return null
  }

  def printBarNotification(text: String, barNum: Int = 14, out: PrintStream = System.out){
    out.print("\n")
    (1 to barNum).foreach((x) => out.print("/"))
    out.print(text)
    (1 to barNum).foreach((x) => out.print("/"))
    out.print("\n\n")
  }

  def cleanAST(node: NodeBase){
    node match{
      case n: ExpressionNode =>{
        n.removeRedudentChildren()
        n.foreach((c) => cleanAST(c))
      }
      case n: NodeBase =>{
        n.foreach((c)=>cleanAST(c))
      }
    }
  }

}
