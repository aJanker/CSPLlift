package de.fosd.typechef.customization

import java.io.{StringWriter, Writer}
import java.lang.management.ManagementFactory
import java.util
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import javax.management.{JMException, ObjectName}

import scala.collection.JavaConverters._

/*
 * A simple thread-safe stopwatch implementation for measuring the elapsed real (wall), user and system time
 * for executing a given code block.
 *
 * Usage:
 *  val (duration, codeReturn) = StopWatch.measure("nameOfTheMeasurement", { some code })
 */
object StopWatch {

    //necessary stuff to query for the process or thread cpu time
    private val mbeanServer = ManagementFactory.getPlatformMBeanServer
    private val osMbean = new ObjectName(ManagementFactory.OPERATING_SYSTEM_MXBEAN_NAME)
    private val threadMXBean = ManagementFactory.getThreadMXBean
    private val PROCESS_CPU_TIME = "ProcessCpuTime"

    private val times: util.Map[(Int, String), Long] = new ConcurrentHashMap[(Int, String), Long]()
    private var currentPeriod: Int = 0

    private def getCurrentPeriod: Int = this.synchronized {
        currentPeriod += 1; currentPeriod
    }

    def reset(): Unit = times.clear()

    def measure[R](block: => R, takeTime: () => Long, convertToUnit: Long => Long = identity): (Long, R) = {
        val t0 = takeTime()
        val result = block
        val t1 = takeTime()

        (convertToUnit(t1 - t0), result)
    }

    def measureCheckpoint[R](checkpoint: String, block: => R, takeTime: () => Long, convertToUnit: Long => Long = identity): (Long, R) = {
        val key = (getCurrentPeriod, checkpoint)
        val (duration, result) = measure(block, takeTime, convertToUnit)
        times.put(key, duration)

        (duration, result)
    }

    def measureSystemTime[R](block: => R): (Long, R) = measure(block, threadMXBean.getCurrentThreadCpuTime, TimeUnit.NANOSECONDS.toMillis)

    def measureSystemTime[R](checkpoint: String, block: => R): (Long, R) = measureCheckpoint(checkpoint, block, threadMXBean.getCurrentThreadCpuTime, TimeUnit.NANOSECONDS.toMillis)

    def measureThreadUserTime[R](block: => R): (Long, R) = measure(block, threadMXBean.getCurrentThreadUserTime, TimeUnit.NANOSECONDS.toMillis)

    def measureThreadUserTime[R](checkpoint: String, block: => R): (Long, R) = measureCheckpoint(checkpoint, block, threadMXBean.getCurrentThreadUserTime, TimeUnit.NANOSECONDS.toMillis)

    def measureProcessCPUTime[R](block: => R): (Long, R) = measure(block, readProcessCPUTime, TimeUnit.NANOSECONDS.toMillis)

    def measureProcessCPUTime[R](checkpoint: String, block: => R): (Long, R) = measureCheckpoint(checkpoint, block, readProcessCPUTime, TimeUnit.NANOSECONDS.toMillis)

    def measureWallTime[R](block: => R): (Long, R) = measure(block, System.currentTimeMillis)

    def measureWallTime[R](checkpoint: String, block: => R): (Long, R) = measureCheckpoint(checkpoint, block, System.currentTimeMillis)

    def get(period: String): Long = times.asScala.find(_._1._2 equals period).map(_._2).getOrElse(0)

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

    override def toString: String = print(new StringWriter()).toString

    def print(writer: Writer): Writer = {
        def writeItem(w: Writer, item: (String, Long)) = w.append("\n(" + item._1 + "\t" + item._2 + ")")

        val items = getMeasurements
        if (items.nonEmpty) writer.append("#stopwatch timings:\n")
        items.foldLeft(writer) { writeItem }
    }

    /**
      * Read the cpu time this process has consumed so far.
      * This relies on a feature of the JVM and the OS
      * that might be not available in all circumstances.
      *
      * @return A time measured in nanoseconds (positive value).
      * @throws JMException If the operation is unsupported.
      */
    def readProcessCPUTime(): Long = {
        val cpuTime: Long = mbeanServer.getAttribute(osMbean, PROCESS_CPU_TIME).asInstanceOf[Long]

        if (cpuTime < 0) // value might be -1 if unsupported
            throw new JMException("Current platform does not support reading the process cpu time")

        cpuTime
    }
}
