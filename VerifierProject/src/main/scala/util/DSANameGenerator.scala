package util

import viper.silver.ast.utility.Consistency

import scala.collection.mutable

/**
  * Created by Severin on 2017-04-05.
  */
class DSANameGenerator {
  val reservedNames: Set[String] = Consistency.reservedNames.toSet
  val separator = "_"
  private val identCounts: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()

  def getVersion(name: String): Int = {
    identCounts.getOrElse(name, -1)
  }

  def setVersion(name: String, version: Int): Unit = {
    identCounts.put(name, version)
  }

  def variableMapSnapshot(): Map[String, Int] = {
    identCounts.toMap
  }

  def makeIdentifier(name: String, count: Int): String = {
    name + separator + count
  }

  def createUniqueIdentifier(name: String): String = {
    var lastCount: Int = identCounts.getOrElse(name, -1)
    if (lastCount >= 0) {
      lastCount += 1
      identCounts.put(name, lastCount)
      makeIdentifier(name, lastCount)
    } else {
      identCounts.put(name, 0)
      makeIdentifier(name, 0)
    }
  }

  def getLastIdentifier(name: String): String = {
    val lastCount: Int = identCounts.getOrElse(name, -1)
    if (lastCount >= 0) {
      makeIdentifier(name, lastCount)
    } else {
      throw new Exception(s"Identifier $name has not been created yet.")
    }
  }

  def increaseVersion(name:String): Unit = {
    val lastCount: Int = identCounts.getOrElse(name, -1)
    if (lastCount >= 0) {
      identCounts.put(name, lastCount + 1)
    } else {
      throw new Exception(s"Identifier $name has not been created yet.")
    }
  }

}
