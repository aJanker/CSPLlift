package de.fosd.typechef.spllift.commons

import java.io.{StringWriter, Writer}
import java.lang.management.ManagementFactory
import java.util
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import scala.collection.JavaConverters._

/*
 * A simple synchronized stopwatch implementation for measure the actual execution time of a given code block.
 *
 * Usage:
 *  val (duration, codeReturn) = StopWatch.measure("nameOfTheMeasurement", { some code })
 */
object StopWatch {

    private lazy val times: util.Map[(Int, String), Long] = new ConcurrentHashMap[(Int, String), Long]()
    private lazy val bean = ManagementFactory.getThreadMXBean
    private var currentPeriodId: Int = 0

    private def genId(): Int = this.synchronized {currentPeriodId += 1; currentPeriodId}

    def measure[R](block: => R): (Long, R) = {
        val t0 = bean.getCurrentThreadUserTime
        val result = block
        val t1 = bean.getCurrentThreadUserTime

        (TimeUnit.NANOSECONDS.toMillis(t1 - t0), result)
    }

    def measure[R](checkpoint: String, block: => R): (Long, R) = {
        val key = (genId(), checkpoint)
        val (time, result) = measure(block)
        times.put(key, time)
        (time, result)
    }

    def get(period: String): Long = times.asScala.find(_._1._2 equals period).map(_._2).getOrElse(-1)

    def getMeasurements: List[(String, Long)] = times.asScala.toList.sortBy(_._1._1).map(x => (x._1._2, x._2))

    def toCSV: String = toCSV(";")
    def toCSV(delimiter: String): String = toCSV(new StringWriter(), delimiter).toString
    def toCSV(writer: Writer, delimiter: String = ";"): Writer = {
        val items = getMeasurements
        if (items.nonEmpty) {
            writer.append(items.map(_._1).mkString(delimiter))
            writer.append("\n")
            writer.append(items.map(_._2.toString).mkString(delimiter))
        }
        writer
    }

    override def toString = print(new StringWriter()).toString
    def print(writer: Writer): Writer = {
        val items = getMeasurements
        if (items.nonEmpty) writer.append("#stopwatch timings:\n")
        items.foldLeft(writer) { (w, i) => w.append("\n(" + i._1 + "\t" + i._2 + ")") }
    }
}
