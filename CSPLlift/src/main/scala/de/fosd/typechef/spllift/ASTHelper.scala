package de.fosd.typechef.spllift

import java.io.File
import java.util

import de.fosd.typechef.crewrite.UsedDefinedDeclaredVariables
import de.fosd.typechef.parser.c.{ASTNavigation, ConditionalNavigation}

import scala.collection.JavaConverters._

trait ASTHelper extends UsedDefinedDeclaredVariables with ASTNavigation with ConditionalNavigation {

    def getFileName(originalFilePath: Option[String]): Option[String] =
        originalFilePath match {
            case None => None
            case Some(path) =>
                if (path.contains(File.separatorChar))
                    Some(path.substring(path.lastIndexOf(File.separatorChar), path.length).replace("/", ""))
                else Some(path)
        }

    /*
     * Creates a java "IdentitySet" as normal java set implementation would remove equal but not identical objects like return statements
     */
    def toJavaIdentitySet[T](c: util.Collection[_ <: T]) : java.util.Set[T] = {
        val res: util.Set[T] = java.util.Collections.newSetFromMap(new util.IdentityHashMap[T, java.lang.Boolean](c.size))
        res.addAll(c)
        res
    }

    def toJavaIdentitySet[T](c: Seq[_ <: T]) : java.util.Set[T] = toJavaIdentitySet(c.asJava)
}
