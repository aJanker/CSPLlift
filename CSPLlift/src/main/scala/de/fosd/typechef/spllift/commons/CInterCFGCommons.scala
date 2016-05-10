package de.fosd.typechef.spllift.commons

import java.util

import de.fosd.typechef.crewrite.UsedDefinedDeclaredVariables
import de.fosd.typechef.parser.c.{AST, ASTNavigation, ConditionalNavigation}

import scala.collection.JavaConverters._

trait CInterCFGCommons extends UsedDefinedDeclaredVariables with ASTNavigation with ConditionalNavigation with TUnitRewriteEngine {

    def getFileName(originalFilePath: Option[String]): Option[String] =
        originalFilePath match {
            case None => None
            case Some(path) => Some(getPlainFileNameS(path))
            /*if (path.contains(File.separatorChar))
                Some(path.substring(path.lastIndexOf(File.separatorChar), path.length).replace("/", ""))
            else Some(path) */
        }

    def getPlainFileName(ast: AST, default: String = "NOFILENAME_AST"): String = getPlainFileNameS(ast.getFile.getOrElse(default))

    def getPlainFileNameS(str: String, default: String = "NOFILENAME"): String = {
        val regex = """^(([^/]+/)*)(([^/.]+)\..+)""".r
        val filePrefix = "file "
        str match {
            case regex(m1, m2, m3, m4) => if (m4.startsWith(filePrefix)) m4.substring(filePrefix.length) else m4
            case _ => default
        }
    }

    /*
     * Creates a java "IdentitySet" as normal java set implementation would remove equal but not identical objects like return statements
     */
    def asJavaIdentitySet[T](c: Seq[_ <: T]): java.util.Set[T] = asJavaIdentitySet(c.asJava)
    def asJavaIdentitySet[T](c: util.Collection[_ <: T]): java.util.Set[T] = {
        val res = java.util.Collections.newSetFromMap(new util.IdentityHashMap[T, java.lang.Boolean](c.size))
        res.addAll(c)
        res
    }
}
