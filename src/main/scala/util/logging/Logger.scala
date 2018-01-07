package util.logging

import java.io.{PrintWriter, BufferedWriter, FileWriter, IOException, File}

//writing own simpile logger to learn scala a bit before doing the compiler.
object Logger {

  def logMsg(msg: String, level : String){
    insureLogOpen
    logStream.println(s"$level $msg")
    logStream.flush()
  }

  def openLog(clear: Boolean) = {
    println(s"Logging to: $logFile")

    try{
      val lFile = new File(logFile)
      if(lFile.isFile && clear){
        lFile.delete()
      }

      logStream = new PrintWriter(new BufferedWriter( new FileWriter(logFile)))
    }
    catch{
      case e : Exception => {
        logStream = null
        println(s"ERROR: Logging is disabled with error: $e.getMessage()")
        throw e
      }
    }
    finally{
    }
  }

  def closeLog(){
    logStream = null
  }

  //insure that log is open.
  def insureLogOpen: Boolean = {
    if(!isLogOpen){
      try{
        openLog(true)
      }
      catch{
        case _ : Throwable => return false
      }
    }

    return true
  }

  def isLogOpen: Boolean = {
    if(logStream == null){
      return false
    }
    return true
  }

  def setLogFilePath(path: String) = logFile = path

  object Level {
    val DEBUG         = "DEBUG:"
    val INFO          = "INFO:"
    val ERROR         = "ERROR:"
    val CERROR        = "CERROR:"
  }

  private var logStream : PrintWriter = null
  private var logFile : String = "./compilator.log"
}
