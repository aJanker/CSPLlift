package de.fosd.typechef.cspllift.commons

import java.io.{StringWriter, Writer}

import scala.collection.mutable

object SolverNotifications {

    private lazy val notifications = new mutable.HashMap[String, Int]()

    def add(notification: String) = {
        val count = notifications.getOrElse(notification, 0) + 1
        notifications.put(notification, count)
    }

    def size(): Int = notifications.size

    def clear() = notifications.clear()

    def issuedNotifications(): Int = notifications.par.values.sum

    override def toString: String = print(new StringWriter()).toString

    def print(writer: Writer): Writer = notifications.foldLeft(writer)((w, notification) => w.append(notification._1 + "\t" + notification._2 + "\n\n"))

}
