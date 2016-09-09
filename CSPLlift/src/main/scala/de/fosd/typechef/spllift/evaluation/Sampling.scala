package de.fosd.typechef.spllift.evaluation

import java.io.{File, FileWriter}

import de.fosd.typechef.conditional.{Choice, One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel, SingleFeatureExpr}
import de.fosd.typechef.parser.c.{AST, TranslationUnit}

import scala.collection.immutable.HashMap
import scala.io.Source

/**
  * Adapted from the sampling infrastructure of Jörg Liebig, Alex von Rhein, and me.
  */
class Sampling(tunit : TranslationUnit, fm: FeatureModel) extends FeatureHelper {

    /** List of all features found in the currently processed file */
    private val features: List[SingleFeatureExpr] = getAllFeatures(tunit)

    /** Maps SingleFeatureExpr Objects to IDs (IDs only known/used in this file) */
    private val featureIDHashmap: Map[SingleFeatureExpr, Int] = new HashMap[SingleFeatureExpr, Int]().++(features.zipWithIndex)

    def codeConfigurationCoverage() : List[SimpleConfiguration] = {
        val configs = configurationCoverage(tunit, fm, features)
        println(configs._2)
        configs._1
    }

    /*
        Configuration Coverage Method copied from Joerg and heavily modified :)
     */
    /**
      * Creates configurations based on the variability nodes found in the given AST.
      * Searches for variability nodes and generates enough configurations to cover all nodes.
      * Configurations do always satisfy the FeatureModel fm.
      * If existingConfigs is non-empty, no config will be created for nodes already covered by these configurations.
      *
      * @param astRoot                           root of the AST
      * @param fm                                The Feature Model
      * @param features                          The set of "interestingFeatures". Only these features will be set in the configs.
      *                                          (Normally the set of all features appearing in the file.)
      * @param existingConfigs                   described above
      * @param preferDisabledFeatures            the sat solver will prefer (many) small configs instead of (fewer) large ones
      * @param includeVariabilityFromHeaderFiles if set to false (default) we will ignore variability in files not ending with ".c".
      *                                          This corresponds to the view of the developer of a ".c" file.
      * @return
      */
    private def configurationCoverage(astRoot: TranslationUnit, fm: FeatureModel, features: List[SingleFeatureExpr],
                              existingConfigs: List[SimpleConfiguration] = List(), preferDisabledFeatures: Boolean = false,
                              includeVariabilityFromHeaderFiles: Boolean = false):
    (List[SimpleConfiguration], String) = {

        val unsatCombinationsCacheFile = new File("unsatCombinationsCache.txt")
        // using this is not correct when different files have different presence conditions
        val useUnsatCombinationsCache = false
        val unsatCombinationsCache: scala.collection.immutable.HashSet[String] = if (useUnsatCombinationsCache && unsatCombinationsCacheFile.exists()) {
            new scala.collection.immutable.HashSet[String] ++ Source.fromFile(unsatCombinationsCacheFile).getLines().toSet
        } else {
            scala.collection.immutable.HashSet()
        }
        var unsatCombinations = 0
        var alreadyCoveredCombinations = 0
        var complexNodes = 0
        var simpleOrNodes = 0
        var simpleAndNodes = 0
        var nodeExpressions: Set[List[FeatureExpr]] = Set()
        def collectAnnotationLeafNodes(root: Any, previousFeatureExprs: List[FeatureExpr] = List(FeatureExprFactory.True), previousFile: String = null) {
            root match {
                case x: Opt[_] => {
                    if (x.condition.equals(previousFeatureExprs.head)) {
                        collectAnnotationLeafNodes(x.entry, previousFeatureExprs, previousFile)
                    } else {
                        collectAnnotationLeafNodes(x.entry, previousFeatureExprs.::(x.condition), previousFile)
                    }
                }
                case x: Choice[_] => {
                    collectAnnotationLeafNodes(x.thenBranch, previousFeatureExprs.::(x.condition), previousFile)
                    collectAnnotationLeafNodes(x.elseBranch, previousFeatureExprs.::(x.condition.not()), previousFile)
                }
                case l: List[_] =>
                    for (x <- l) {
                        collectAnnotationLeafNodes(x, previousFeatureExprs, previousFile)
                    }
                case x: AST => {
                    val newPreviousFile = if (x.getFile.isDefined) x.getFile.get else previousFile
                    if (x.productArity == 0) {
                        // termination point of recursion
                        if (includeVariabilityFromHeaderFiles ||
                            (newPreviousFile != null && newPreviousFile.endsWith(".c"))) {
                            if (!nodeExpressions.contains(previousFeatureExprs)) {
                                nodeExpressions += previousFeatureExprs
                            }
                        }
                    } else {
                        for (y <- x.productIterator.toList) {
                            collectAnnotationLeafNodes(y, previousFeatureExprs, newPreviousFile)
                        }
                    }
                }
                case Some(x) => {
                    collectAnnotationLeafNodes(x, previousFeatureExprs, previousFile)
                }
                case None => {}
                case One(x) => {
                    collectAnnotationLeafNodes(x, previousFeatureExprs, previousFile)
                }
                case o => {
                    // termination point of recursion
                    if (includeVariabilityFromHeaderFiles ||
                        (previousFile != null && previousFile.endsWith(".c"))) {
                        if (!nodeExpressions.contains(previousFeatureExprs)) {
                            nodeExpressions += previousFeatureExprs
                        }
                    }
                }
            }
        }
        collectAnnotationLeafNodes(astRoot, List(FeatureExprFactory.True), if (astRoot.getFile.isDefined) astRoot.getFile.get else null)

        // now optNodes contains all Opt[..] nodes in the file, and choiceNodes all Choice nodes.
        // True node never needs to be handled
        val handledExpressions = scala.collection.mutable.HashSet(FeatureExprFactory.True)
        var retList: List[SimpleConfiguration] = List()
        //inner function
        def handleFeatureExpression(fex: FeatureExpr) = {
            if (!handledExpressions.contains(fex) && !(useUnsatCombinationsCache && unsatCombinationsCache.contains(fex.toTextExpr))) {
                //println("fex : " + fex.toTextExpr)
                // search for configs that imply this node
                var isCovered: Boolean = false
                fex.getConfIfSimpleAndExpr() match {
                    case None => {
                        fex.getConfIfSimpleOrExpr() match {
                            case None => {
                                complexNodes += 1
                                isCovered = (retList ++ existingConfigs).exists(
                                    {
                                        conf: SimpleConfiguration => conf.toFeatureExpr.implies(fex).isTautology(fm)
                                    }
                                )
                            }
                            case Some((enabled: Set[SingleFeatureExpr], disabled: Set[SingleFeatureExpr])) => {
                                simpleOrNodes += 1
                                isCovered = (retList ++ existingConfigs).exists({
                                    conf: SimpleConfiguration => conf.containsAtLeastOneFeatureAsEnabled(enabled) ||
                                        conf.containsAtLeastOneFeatureAsDisabled(disabled)
                                })
                            }
                        }
                    }
                    case Some((enabled: Set[SingleFeatureExpr], disabled: Set[SingleFeatureExpr])) => {
                        simpleAndNodes += 1
                        isCovered = (retList ++ existingConfigs).exists({
                            conf: SimpleConfiguration => conf.containsAllFeaturesAsEnabled(enabled) &&
                                conf.containsAllFeaturesAsDisabled(disabled)
                        })
                    }
                }
                if (!isCovered) {
                    val completeConfig = completeConfiguration(fex, features, fm, preferDisabledFeatures)
                    if (completeConfig != null) {
                        retList ::= completeConfig
                        //println("created config for fex " + fex.toTextExpr)
                    } else {
                        if (useUnsatCombinationsCache) {
                            //unsatCombinationsCacheFile.getParentFile.mkdirs()
                            val fw = new FileWriter(unsatCombinationsCacheFile, true)
                            fw.write(fex.toTextExpr + "\n")
                            fw.close()
                        }
                        unsatCombinations += 1
                        //println("no satisfiable configuration for fex " + fex.toTextExpr)
                    }
                } else {
                    //println("covered fex " + fex.toTextExpr)
                    alreadyCoveredCombinations += 1
                }
                handledExpressions.add(fex)
                //println("retList.size = " + retList.size)
            }
        }
        if (nodeExpressions.isEmpty ||
            (nodeExpressions.size == 1 && nodeExpressions.head.equals(List(FeatureExprFactory.True)))) {
            // no feature variables in this file, build one random config and return it
            val completeConfig = completeConfiguration(FeatureExprFactory.True, features, fm, preferDisabledFeatures)
            if (completeConfig != null) {
                retList ::= completeConfig
                //println("created config for fex " + fex.toTextExpr)
            } else {
                if (useUnsatCombinationsCache) {
                    //unsatCombinationsCacheFile.getParentFile.mkdirs()
                    val fw = new FileWriter(unsatCombinationsCacheFile, true)
                    fw.write(FeatureExprFactory.True + "\n")
                    fw.close()
                }
                unsatCombinations += 1
                //println("no satisfiable configuration for fex " + fex.toTextExpr)
            }
        } else {
            for (featureList: List[FeatureExpr] <- nodeExpressions) {
                val fex: FeatureExpr = featureList.fold(FeatureExprFactory.True)(_ and _)
                handleFeatureExpression(fex)
            }
        }
        def getFeaturesInCoveredExpressions: Set[SingleFeatureExpr] = {
            // how many features have been found in this file (only the .c files)?
            var features: Set[SingleFeatureExpr] = Set()
            for (exLst <- nodeExpressions)
                for (ex <- exLst)
                    for (feature <- ex.collectDistinctFeatureObjects)
                        features += feature
            features
        }

        (retList,
            " unsatisfiableCombinations:" + unsatCombinations + "\n" +
                " already covered combinations:" + alreadyCoveredCombinations + "\n" +
                " created combinations:" + retList.size + "\n" +
                (if (!includeVariabilityFromHeaderFiles) " Features in CFile: " + getFeaturesInCoveredExpressions.size + "\n" else "") +
                " found " + nodeExpressions.size + " NodeExpressions\n" +
                " found " + simpleAndNodes + " simpleAndNodes, " + simpleOrNodes + " simpleOrNodes and " + complexNodes + " complex nodes.\n")
    }

    /**
      * Optimzed version of the completeConfiguration method. Uses FeatureExpr.getSatisfiableAssignment to need only one SAT call.
      * @param expr input feature expression
      * @param list list of features
      * @param model input feature model
      * @return
      */
    def completeConfiguration(expr: FeatureExpr, list: List[SingleFeatureExpr], model: FeatureModel, preferDisabledFeatures: Boolean = false): SimpleConfiguration = {
        expr.getSatisfiableAssignment(model, list.toSet, preferDisabledFeatures) match {
            case Some(ret) => new SimpleConfiguration(ret._1, ret._2, features, featureIDHashmap)
            case None => null
        }
    }
}

// representation of a product configuration that can be dumped into a file
// and loaded at further runs
class SimpleConfiguration(private val features: List[SingleFeatureExpr], private val featureIDHashmap: Map[SingleFeatureExpr, Int], private val config: scala.collection.immutable.BitSet) extends scala.Serializable {

    def this(trueSet: List[SingleFeatureExpr], falseSet: List[SingleFeatureExpr], features: List[SingleFeatureExpr], featureIDHashmap: Map[SingleFeatureExpr, Int]) =
        this(features, featureIDHashmap,
            {
                val ret: scala.collection.mutable.BitSet = scala.collection.mutable.BitSet()
                for (tf: SingleFeatureExpr <- trueSet) ret.add(featureIDHashmap(tf))
                for (ff: SingleFeatureExpr <- falseSet) ret.remove(featureIDHashmap(ff))
                ret.toImmutable
            }
        )

    def getTrueSet: Set[SingleFeatureExpr] = {
        features.filter({
            fex: SingleFeatureExpr => config.apply(featureIDHashmap(fex))
        }).toSet
    }

    def getFalseSet: Set[SingleFeatureExpr] = {
        features.filterNot({
            fex: SingleFeatureExpr => config.apply(featureIDHashmap(fex))
        }).toSet
    }

    override def toString: String = {
        features.map(
            {
                fex: SingleFeatureExpr => if (config.apply(featureIDHashmap(fex))) fex else fex.not()
            }
        ).mkString("&&")
    }

    // caching, values of this field will not be serialized
    @transient
    private var featureExpression: FeatureExpr = null

    def toFeatureExpr: FeatureExpr = {
        if (featureExpression == null)
            featureExpression = FeatureExprFactory.createFeatureExprFast(getTrueSet, getFalseSet)
        featureExpression
    }

    /**
      * This method assumes that all features in the parameter-set appear in either the trueList, or in the falseList
      * @param features given feature set
      * @return
      */
    def containsAllFeaturesAsEnabled(features: Set[SingleFeatureExpr]): Boolean = {
        for (fex <- features) {
            if (!config.apply(featureIDHashmap(fex))) return false
        }
        true
    }

    /**
      * This method assumes that all features in the parameter-set appear in the configuration (either as true or as false)
      * @param features given feature set
      * @return
      */
    def containsAllFeaturesAsDisabled(features: Set[SingleFeatureExpr]): Boolean = {
        for (fex <- features) {
            if (config.apply(featureIDHashmap(fex))) return false
        }
        true
    }

    def containsAtLeastOneFeatureAsEnabled(set: Set[SingleFeatureExpr]): Boolean =
        !containsAllFeaturesAsDisabled(set)

    def containsAtLeastOneFeatureAsDisabled(set: Set[SingleFeatureExpr]): Boolean =
        !containsAllFeaturesAsEnabled(set)

    override def equals(other: Any): Boolean = {
        if (!other.isInstanceOf[SimpleConfiguration]) super.equals(other)
        else {
            val otherSC = other.asInstanceOf[SimpleConfiguration]
            otherSC.config.equals(this.config)
        }
    }

    override def hashCode(): Int = config.hashCode()
}
