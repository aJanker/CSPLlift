package de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.extractors

import de.fosd.typechef.cmodule.CModule
import de.fosd.typechef.cmodule.ccallgraph.cpointeraliasing.types.{Assignment, ObjectName}
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.parser.c.AST
import org.slf4j.{Logger, LoggerFactory}

object DefaultExtractor {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    /**
      * Extracts all pointer related object names and assignments from a given AST.
      *
      * Note: Smelly Pointer voodoo, such as &(x + y), is not supported.
      */
    def extractAll(node: AST, env: CModule): (Set[Opt[ObjectName]], Set[Opt[Assignment]]) = {
        val (objectNamesRuntime, objectNames) =
            StopWatch.measureThreadUserTime({
                ObjectNameExtractor.get(node, env)
            })
        val (assignmentsRuntime, assignments) =
            StopWatch.measureThreadUserTime({
                AssignmentExtractor.get(node, env)
            })
        logger.debug("Extracted ObjectNames in " + objectNamesRuntime + "ms.")
        logger.debug("Extracted Assignments in " + assignmentsRuntime + "ms.")
        (objectNames.toSet, assignments)
    }

}
