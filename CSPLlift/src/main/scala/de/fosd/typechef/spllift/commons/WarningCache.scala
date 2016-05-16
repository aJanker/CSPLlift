package de.fosd.typechef.spllift.commons

import java.io.{StringWriter, Writer}

import scala.collection.mutable

object WarningCache {

    private lazy val warningsMap = new mutable.HashMap[String, Int]()

    def add(warning : String) = {
        val count = warningsMap.getOrElse(warning, 0) + 1
        warningsMap.put(warning, count)
    }

    def size() : Int = warningsMap.size

    def issuedWarnings() : Int = warningsMap.par.reduceLeft((l, r) => l._2 + r._2)

    override def toString : String = print(new StringWriter()).toString

    def print(writer : Writer) : Writer = warningsMap.foldLeft(writer)((w, warning) => w.append(warning._1 + "\t" + warning._2))

}
