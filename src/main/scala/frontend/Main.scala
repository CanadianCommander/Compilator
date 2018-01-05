package frontend

import util.logging.Logger._

object Main {
  def main(args: Array[String]) = {
    //parse args some where up here

    //start logging
    openLog(true)
    logMsg("HELLO YOU GUYS", Level.DEBUG)
    //compile down here

  }
}
