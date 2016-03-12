package de.fosd.typechef.spllift

import java.io.File

import de.fosd.typechef.parser.c.{ASTNavigation, ConditionalNavigation}

trait ASTHelper extends ASTNavigation with ConditionalNavigation {

    def getFileName(originalFilePath: Option[String]): Option[String] =
        originalFilePath match {
            case None => None
            case Some(path) =>
                if (path.contains(File.separatorChar))
                    Some(path.substring(path.lastIndexOf(File.separatorChar), path.length).replace("/", ""))
                else Some(path)
        }


}
