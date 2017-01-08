package de.fosd.typechef.cspllift.cintercfg

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.typesystem.modulelinking.CLinkingExtractor
import heros.InterproceduralCFG
import spllift.ConditionalEdgeFunction

/**
  * Extensions to the InterproceduralCFG of the IFDS Framework used in TypeChef
  */
trait CIFDSInterCFG[N, M] extends InterproceduralCFG[N, M] {

    /**
      * Returns all functions which are considered entry functions in the programm flow (such as main)
      */
    def getEntryFunctions: List[M]

    /**
      * Retrieves the TypeChef FeatureModel
      */
    def getFeatureModel: FeatureModel

    /**
      * Retrieves the configuration options of the cfg.
      */
    def getOptions: CInterCFGConfiguration

    /**
      * Retrieves the cfg contraint of a given node.
      */
    def getCondition(node: N): FeatureExpr

    /**
      * Gets the context of a given node.
      */
    def getASTEnv(node: N): ASTEnv

    /**
      * Gets the translationunit containing a node.
      */
    def getTUnit(node: N): TranslationUnit

    /**
      * Gets the typesystem of a given node.
      */
    def getTS(node: N): CTypeSystemFrontend with CLinkingExtractor

    /**
      * Gets the correctly annotated (lifted) call edge condition between a call site and its corresponding target callee.
      * With the use of this method, SPLlift is able to resolve the constraint of an call and call-to-return edge.
      *
      * @param callSite the statement where the call occurred
      * @param callee   the target of the call
      * @return the correct points to flow constraint
      */
    def getPointsToCondition(callSite: N, callee: M): FeatureExpr

    /**
      * Gets the correctly annotated (lifted) call edge function for a single statement.
      * With the use of this method, SPLlift is able to introduce initial seeds according to their presence conditions
      *
      * @param stmt the statement (generally a global variable)
      * @return the correct conditional flow edge
      */
    def getConditionalEdgeFunction(stmt: N): ConditionalEdgeFunction

    /**
      * Gets the correctly annotated (lifted) call edge function between a call site and its corresponding target callee.
      * With the use of this method, SPLlift is able to resolve the constraint of an call and call-to-return edge.
      *
      * @param callSite the statement where the call occurred
      * @param callee   the target of the call
      * @return the correct points-to conditional flow edge
      */
    def getConditionalEdgeFunction(callSite: N, callee: M): ConditionalEdgeFunction

    /**
      * Gets the full flow condition between a source node and its successor node.
      *
      * @param srcStmt  the source statement
      * @param succStmt the successor statement
      * @return the correct flow constraint
      */
    def getSuccFlowCondition(srcStmt: N, succStmt: N): FeatureExpr

    /**
      * Gets the full flow condition between a source node and its successor node.
      *
      * @param srcStmt  the source statement
      * @param predStmt the predcessor statement
      * @return the correct flow constraint
      */
    def getPredFlowCondition(srcStmt: N, predStmt: N): FeatureExpr
}

