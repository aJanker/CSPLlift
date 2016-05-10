package de.fosd.typechef.spllift.commons

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

    def measure[R](checkpoint : String, block: => R): (Long, R) = {
        val key = (genId(), checkpoint)
        val (time, result) = measure(block)
        times.put(key, time)
        (time, result)
    }

    def get(period: String): Long = times.asScala.find(_._1._2 equals period).map(_._2).getOrElse(-1)

    override def toString = {
        var res = "#stopwatch timings "
        val swItems = times.asScala.toList.sortBy(_._1._1)

        if (swItems.size > 0) {
            res = res + "("
            res = res + swItems.map(_._1._2).reduce(_ + ", " + _)
            res = res + ")\n"
            res = res + swItems.map(_._2.toString).reduce(_ + ";" + _)
        }
        res
    }

}
