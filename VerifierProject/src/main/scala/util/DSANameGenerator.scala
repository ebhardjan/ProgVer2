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
  private val identMaxCounts: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()
  private val unversionedVariables: mutable.Set[String] = mutable.Set[String]()

  /** Get the latest version number of a variable. Return -1 if the variable has not been used before.
    */
  def getVersion(name: String): Int = {
    identCounts.getOrElse(name, -1)
  }

  /** Set the version of a variable to a given value.
    */
  def setVersion(name: String, version: Int): Unit = {
    if (version < 0) throw new Exception("Cant set variable version below 0.")
    identCounts.put(name, version)
    val maxCount = identMaxCounts.getOrElse(name, -1)
    if (maxCount < version) identMaxCounts.put(name, version)
  }

  def putUnversionedVariable(variable: String): Unit = {
    unversionedVariables.add(variable)
  }

  /** Update all versions in the given map to the value they map to.
    */
  def bulkUpdateVersions(versionMap: Map[String, Int]): Unit = {
    for ((key, version) <- versionMap if identCounts.contains(key)) {
      setVersion(key, version)
    }
  }

  /** Return an immutable copy of the mapping from variables to their latest version number.
    * If a set of names is provided, only return the versions of those.
    */
  def variableMapSnapshot(subset: Set[String] = null, useMaxCounts: Boolean = false): Map[String, Int] = {
    val map = if (useMaxCounts) identMaxCounts else identCounts
    if (subset == null) {
      map.toMap
    } else {
      map.filter(p => subset.contains(p._1)).toMap
    }
  }

  /** Create a new variable identifier from a variable name and the version number.
    */
  def makeIdentifier(name: String, version: Int): String = {
    name + separator + version
  }

  /** Create a new unique identifier for the given variable name.
    * Note: if the version number has been set using [[setVersion()]], it might not actually be unique.
    */
  def createUniqueIdentifier(name: String): String = {
    var lastCount: Int = identCounts.getOrElse(name, -1)
    if (lastCount >= 0) {
      lastCount += 1
      setVersion(name, lastCount)
      makeIdentifier(name, lastCount)
    } else {
      setVersion(name, 0)
      makeIdentifier(name, 0)
    }
  }

  /** Return the last created unique identifier.
    */
  def getLastIdentifier(name: String): String = {
    val lastCount: Int = identCounts.getOrElse(name, -1)
    if (lastCount >= 0) {
      makeIdentifier(name, lastCount)
    } else {
      if (unversionedVariables.contains(name)) {
        name
      } else {
        throw new Exception(s"Identifier $name has not been created yet.")
      }
    }
  }

  /** Increase the version number of a given variable.
    */
  def increaseVersion(name:String): Unit = {
    val lastCount: Int = identCounts.getOrElse(name, -1)
    if (lastCount >= 0) {
      setVersion(name, lastCount + 1)
    } else {
      throw new Exception(s"Identifier $name has not been created yet.")
    }
  }

  /** Increase the version number of all given variables.
    */
  def increaseVersion(nameSet: Set[String]): Unit = {
    for (name <- nameSet) increaseVersion(name)
  }

}
