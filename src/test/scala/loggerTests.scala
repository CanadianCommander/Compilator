import org.scalatest._
import java.io.{BufferedReader, FileReader}

import util.logging.Logger._

class LoggerTests extends FlatSpec {
  "log" should "open without exceptions" in {
    openLog(true)
  }

  "log" should "log messages correctly" in {
    closeLog()
    setLogFilePath("foobar.log")
    openLog(true)

    logMsg("HELLO YOU GUYS", Level.DEBUG)

    var reader: BufferedReader = new BufferedReader(new FileReader("foobar.log"))
    var logLine: String = reader.readLine()
    assert(logLine.compareTo("DEBUG: HELLO YOU GUYS") == 0)
  }
}
