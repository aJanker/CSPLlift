package de.fosd.typechef.spllift.commons

import java.io.{StringWriter, Writer}
import java.lang.management.ManagementFactory
import java.util
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import scala.collection.JavaConverters._

/*
 * A simple thread-safe stopwatch implementation for measuring the elapsed real (wall), user and system time
 * for executing a given code block.
 *
 * Usage:
 *  val (duration, codeReturn) = StopWatch.measure("nameOfTheMeasurement", { some code })
 */
object StopWatch {

    private lazy val times: util.Map[(Int, String), Long] = new ConcurrentHashMap[(Int, String), Long]()
    private lazy val bean = ManagementFactory.getThreadMXBean
    private var currentPeriodId: Int = 0

    private def genId(): Int = this.synchronized {currentPeriodId += 1; currentPeriodId}

    def reset() = times.clear()

    def measure[R](block: => R, takeTime: () => Long, convertToUnit : Long => Long = identity): (Long, R) = {
        val t0 = takeTime()
        val result = block
        val t1 = takeTime()

        (convertToUnit(t1 - t0), result)
    }

    def measureCheckpoint[R](checkpoint: String, block: => R, takeTime: () => Long, convertToUnit : Long => Long = identity): (Long, R) = {
        val key = (genId(), checkpoint)
        val (duration, result) = measure(block, takeTime, convertToUnit)
        times.put(key, duration)

        (duration, result)
    }

    def measureSystemTime[R](block: => R): (Long, R) = measure(block, bean.getCurrentThreadCpuTime, TimeUnit.NANOSECONDS.toMillis)
    def measureSystemTime[R](checkpoint: String, block: => R): (Long, R) = measureCheckpoint(checkpoint, block, bean.getCurrentThreadCpuTime, TimeUnit.NANOSECONDS.toMillis)

    def measureUserTime[R](block: => R): (Long, R) = measure(block, bean.getCurrentThreadUserTime, TimeUnit.NANOSECONDS.toMillis)
    def measureUserTime[R](checkpoint: String, block: => R): (Long, R) = measureCheckpoint(checkpoint, block, bean.getCurrentThreadUserTime, TimeUnit.NANOSECONDS.toMillis)

    def measureWallTime[R](block: => R): (Long, R) = measure(block, System.currentTimeMillis)
    def measureWallTime[R](checkpoint: String, block: => R): (Long, R) = measureCheckpoint(checkpoint, block, System.currentTimeMillis)

    def get(period: String): Long = times.asScala.find(_._1._2 equals period).map(_._2).getOrElse(-1)

    def getMeasurements: List[(String, Long)] = times.asScala.toList.sortBy(_._1._1).map(x => (x._1._2, x._2))

    def toCSV: String = toCSV("; ")
    def toCSV(delimiter: String): String = toCSV(new StringWriter(), delimiter).toString
    def toCSV(writer: Writer, delimiter: String = "; "): Writer = {
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
        def writeItem(w : Writer, item : (String, Long)) =  w.append("\n(" + item._1 + "\t" + item._2 + ")")
        val items = getMeasurements
        if (items.nonEmpty) writer.append("#stopwatch timings:\n")
        items.foldLeft(writer) { writeItem }
    }
}
