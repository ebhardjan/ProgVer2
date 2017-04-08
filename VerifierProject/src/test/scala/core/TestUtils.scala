package core

import java.io.File

/**
  * Created by jan on 07.04.17.
  *
  * Testing utility methods
  */
object TestUtils {

  /**
    * Get a list of all the vpr files in a given directory
    *
    * @param directoryPath directory where to look for vpr files
    * @return list of paths of the vpr files
    */
  def getListOfVprFiles(directoryPath: String): List[String] = {
    val directory = new File(directoryPath)
    if (directory.exists && directory.isDirectory) {
      directory.listFiles
        .filter(f => f.isFile)
        .filter(f => f.getName.contains(".vpr"))
        .map(f => f.getName)
        .toList
    } else {
      List()
    }
  }

}
