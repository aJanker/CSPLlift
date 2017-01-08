package de.fosd.typechef.cmodule.ccallgraph.clinking

import java.io.File

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel}
import de.fosd.typechef.typesystem.linker._
import de.fosd.typechef.typesystem.modulelinking.CLinkMap
import org.slf4j.{Logger, LoggerFactory}

/**
  * Facade for generating module linking information for a whole project.
  */
object CModuleLinkMapGenerator extends App with CModuleLinkCommons {

    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    val startDir = args(0)
    val featureModel_DIMACS: String = if (args.length > 3) args(1) else " "

    val fm =
        if (new File(featureModel_DIMACS).isFile) FeatureExprFactory.default.featureModelFactory.createFromDimacsFilePrefix(featureModel_DIMACS, "")
        else FeatureExprFactory.default.featureModelFactory.empty

    mergeAndWriteMaps(startDir, fm)

    def mergeMaps(rootDir: String, fm: FeatureModel = FeatureExprFactory.default.featureModelFactory.empty, strictness: Strictness = LINK_NAMEONLY): (CLinkMap, CLinkMap) = {
        val varFileList = getFileTree(new File(rootDir)).filter(f => f.isFile && f.getPath.endsWith(VAR_LINKMAP_FILE_EXTENSION))
        val fDefFileList = getFileTree(new File(rootDir)).filter(f => f.isFile && f.getPath.endsWith(FDEF_LINKMAP_FILE_EXTENSION))

        val varMap = varFileList.foldLeft(new CLinkMap()) {
            case (map, file) => map.merge(CLinkMap.readFromFile(file.getAbsolutePath))
        }

        val fDefMap = fDefFileList.foldLeft(new CLinkMap()) {
            case (map, file) => map.merge(CLinkMap.readFromFile(file.getAbsolutePath))
        }

        (fDefMap, varMap)
    }

    def mergeAndWriteMaps(rootDir: String, fm: FeatureModel = FeatureExprFactory.default.featureModelFactory.empty, interfaceFile: Option[String] = None): Unit = {
        def getOutput(fileExtension: String) =
            interfaceFile match {
                case Some(path) => new File(path)
                case None if checkIfModuleDirExistsElseCreate => new File(getModuleDir.getAbsolutePath + File.separatorChar + MODULE_INTERFACE_FILENAME + fileExtension)
                case _ => new File(rootDir + File.separatorChar + MODULE_INTERFACE_FILENAME + fileExtension)
            }

        val fDefOut = getOutput(FDEF_LINKMAP_FILE_EXTENSION)
        val varOut = getOutput(VAR_LINKMAP_FILE_EXTENSION)

        val (fDefMap, varMap) = mergeMaps(rootDir, fm)

        CLinkMap.writeToFile(fDefOut.getAbsolutePath, fDefMap)
        CLinkMap.writeToFile(varOut.getAbsolutePath, varMap)

        logger.info("Var linking map written to:\t" + varOut.getAbsolutePath)
        logger.info("FDef linking map written to:\t" + fDefOut.getAbsolutePath)
    }

    private def getFileTree(f: File): Stream[File] = f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree) else Stream.empty)

}
