package de.fosd.typechef.cmodule.ccallgraph.clinking

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.customization.StopWatch
import de.fosd.typechef.error.Position
import de.fosd.typechef.typesystem.modulelinking.{CLinkMap, CLinkingName}
import org.slf4j.{Logger, LoggerFactory}

class CModuleLinkMap(rootDir: String) extends CModuleLinkCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private val (loadTime, (varLinkMap, fDefLinkMap)) =
        StopWatch.measureProcessCPUTime({
            (CLinkMap.readFromFile(getCModuleLinkMapPath(rootDir, VAR_LINKMAP_FILE_EXTENSION)), CLinkMap.readFromFile(getCModuleLinkMapPath(rootDir, FDEF_LINKMAP_FILE_EXTENSION)))
        })

    logger.info("Loaded linking map in " + loadTime + "ms")

    /**
      * Finds all export positions of a input function name.
      */
    def findFDefsExportLocations(name: String): List[Opt[Position]] = findPositions(name, fDefLinkMap)

    /**
      * Finds all export positions of a input function name declared in a certain header.
      */
    def findFDefsExportLocations(name: String, declFile: String): List[Opt[Position]] = findPositions(name, declFile, fDefLinkMap)

    /**
      * Finds all export positions of a input function name declared in a certain header.
      */
    def findFDefsExportLocations(linkName: CLinkingName): List[Opt[Position]] = findPositions(linkName.name, linkName.file, fDefLinkMap)

    /**
      * Finds all export positions of a input variable name declared in a certain header.
      */
    def findVarExportLocations(linkName: CLinkingName): List[Opt[Position]] = findPositions(linkName.name, linkName.file, varLinkMap)

    /**
      * Finds all export positions of a input variable name declared in a certain header.
      */
    def findVarExportLocations(name: String, declFile: String): List[Opt[Position]] = findPositions(name, declFile, varLinkMap)

    /**
      * Finds all export positions of a input variable name declared in a certain header.
      */
    def findVarExportLocations(name: String): List[Opt[Position]] = findPositions(name, varLinkMap)

    private def findPositions(name: String, declFile: String, map: CLinkMap): List[Opt[Position]] = {
        // workaround for "file " prefix
        lazy val declFilePath = removeFilePrefix(declFile)
        val allExportsByName = map.getExportsByName(name)

        // no corresponding export -> empty list
        if (allExportsByName.isEmpty) {
            if (logger.isDebugEnabled)
                logger.debug("No corresponding export found for name:\t" + name)
            return List()
        }

        val exports = {
            if (allExportsByName.get.contains(declFile)) Some(allExportsByName.get(declFile))
            else if (allExportsByName.get.contains(declFilePath)) Some(allExportsByName.get(declFilePath))
            else None
        }

        if (exports.isEmpty) {
            if (logger.isDebugEnabled) logger.debug("No corresponding export found for name:\t" + name + " with decl @ " + declFilePath)
            return List()
        }

        exports.get.flatMap(sig => sig.classicSignature.pos.map(Opt(sig.classicSignature.fexpr, _)))
    }

    private def findPositions(name: String, map: CLinkMap): List[Opt[Position]] = {
        val allExportsByName = map.getExportsByName(name)

        // no corresponding export -> empty list
        if (allExportsByName.isEmpty) {
            if (logger.isDebugEnabled) logger.debug("No export found for name:\t" + name)
            return List()
        }

        val exportsByName = allExportsByName.get
        val exports = exportsByName.flatMap(_._2).toList

        exports.flatMap(sig => sig.classicSignature.pos.map(Opt(sig.classicSignature.fexpr, _)))
    }
}
