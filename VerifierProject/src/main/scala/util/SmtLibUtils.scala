package util

import smtlib.parser.Terms.Sort

/**
  * Created by jan on 08.04.17.
  */
object SmtLibUtils {

  def declarationString(declarations: Map[String, Sort]): String = {
    val stringBuilder = new StringBuilder
    for ((name, sort) <- declarations) stringBuilder.append(f"(declare-const $name ($sort))\n")
    stringBuilder.toString()
  }

}
