package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfunction

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{InformationFlowFact, Zero}
import de.fosd.typechef.cspllift.cintercfg._
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory
import de.fosd.typechef.parser.c.{AST, ASTEnv, FunctionDef, Id}
import de.fosd.typechef.typesystem._
import org.slf4j.{Logger, LoggerFactory}


abstract class IFDefaultFlowFunction(interproceduralCFG: CInterCFG, curr: CInterCFGNode, succ: CInterCFGNode) extends IFFlowFunction {
    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    private[informationflow] lazy val currStmt: AST = curr.get
    private[informationflow] lazy val succStmt: AST = succ.get

    // Lazy caching of some repeatedly used variables
    private[informationflow] lazy val currASTEnv: ASTEnv = interproceduralCFG.getASTEnv(curr)
    private[informationflow] lazy val currOpt: Opt[AST] = parentOpt(currStmt, currASTEnv).asInstanceOf[Opt[AST]]
    private[informationflow] lazy val currTS: CTypeSystemFrontend with CTypeCache with CDeclUse = interproceduralCFG.getTS(curr)

    private[informationflow] lazy val succTS: CTypeSystemFrontend with CTypeCache with CDeclUse with CEnvCache = interproceduralCFG.getTS(succ)
    private[informationflow] lazy val succVarEnv = currStmt match {
        case f: FunctionDef => succTS.lookupEnv(f.stmt.innerStatements.headOption.getOrElse(currOpt).entry)
        case _ => succTS.lookupEnv(succStmt)
    }

    private[informationflow] lazy val currDefines: List[Id] = defines(currStmt)
    private[informationflow] lazy val currUses: List[Id] = uses(currStmt)
    private[informationflow] lazy val currAssignments: List[(Id, List[Id])] = assignsVariables(currStmt)
    private[informationflow] lazy val currStructFieldAssigns: List[(Id, List[Id])] = assignsField(currStmt)
    private[informationflow] lazy val currStructFieldUses: List[(Id, List[Id])] = usesField(currStmt)
    private[informationflow] lazy val currStatementIsAssignment: Boolean = currDefines.nonEmpty || currAssignments.nonEmpty || currStructFieldAssigns.nonEmpty
    private[informationflow] lazy val currSatisfiableCondition: Set[String] = currSatisfiableAssignment.getOrElse(defaultSatisfiableAssignment)._1.map(_.feature).toSet
    private[informationflow] lazy val defineSources: List[SourceDefinition] = currDefines.flatMap(genSource)
    private[informationflow] lazy val assignmentSources: List[SourceDefinition] =
        if (currStructFieldAssigns.isEmpty) currAssignments.flatMap { case (assignee, _) => genSource(assignee) }
        else currStructFieldAssigns.flatMap { case (field, parents) => genSourceForField(field, parents, getCurrentScope(parents.lastOption.getOrElse(field))) }
    private lazy val defaultSatisfiableAssignment = (BDDFeatureExprFactory.TrueB.collectDistinctFeatureObjects, BDDFeatureExprFactory.FalseB.collectDistinctFeatureObjects)
    private lazy val currSatisfiableAssignment = currASTEnv.featureExpr(currStmt).getSatisfiableAssignment(interproceduralCFG.getFeatureModel, currOpt.condition.collectDistinctFeatureObjects, preferDisabledFeatures = false)

    override def computeSink(s: Sink): util.Set[InformationFlowFact] = if (interproceduralCFG.isStartPoint(curr)) GEN(s) else KILL

    override def computeZero(z: Zero): util.Set[InformationFlowFact] = GEN(z)

    private[informationflow] def getDefineSourcesFromAssignment(define: Id): List[SourceDefinition] = assignmentSources.filter(_.getType.getName.equals(define))

    private[informationflow] def currFactIsAssignee(fact: Source): Boolean =
        fact match {
            case s: Source if (s.getType match {
                case Struct(_, Some(_)) => true
                case _ => false
            }) => currStructFieldAssigns.exists(assignment => isFullFieldMatch(fact, assignment) || isPartFieldMatch(fact, assignment))
            case s: Source => currDefines.exists(s.getType.getName.equals) || currAssignments.exists { case (assignee, _) => s.getType.getName.equals(assignee) }
            case _ => false
        }

    private[informationflow] def getSinksAndSourcesOf(currSourceDefinition: SourceDefinition, sources: List[SourceDefinition]): List[InformationFlowFact] = {
        val sourcesOf = sources.flatMap {
            case s: Source => Some(SourceDefinitionOf(s.getType, curr, currSourceDefinition, s.getScope))
            case _ => None
        }

        val sinks = sources.map(genSource => SinkToAssignment(curr, currSourceDefinition, genSource.getType.getName))

        sourcesOf ::: sinks
    }

    private[informationflow] def genSourceForField(field: Id, parents: List[Id], scopes: List[Int]): List[SourceDefinition] = {
        val fieldSources = genFieldSource(field, parents)
        val parentSources =
            if (parents.nonEmpty) scopes.flatMap(scope => parents.tail.flatMap(genStructSource(scope)) ::: genSource(parents.head) ::: Nil)
            else List()

        val result = fieldSources.map(cFieldSource =>
            parentSources.foldLeft(cFieldSource) {
                case (fieldSource, s: Source) => SourceDefinition(Struct(s.getType.getName, Some(fieldSource)), s.getCIFGStmt, s.getScope)
                case (fieldSource, _) => fieldSource
            })

        result
    }

    private[informationflow] def genStructSource(scope: Int)(define: Id): List[SourceDefinition] = List(SourceDefinition(Struct(define, None), curr, scope))

    private def genSource(define: Id): List[SourceDefinition] = {
        val scopes = getCurrentScope(define)
        val res = scopes.flatMap(scope => singleVisitOnSourceTypes(define, succVarEnv.varEnv, genStructSource(scope), genVarSource(scope)))
        res
    }

    private[informationflow] def getCurrentScope(id: Id): List[Int] = {
        val scopes = succVarEnv.varEnv.lookupScope(id.name).toOptList.map(_.entry).filter(!_.equals(SCOPE_UNKNOWN)) // our analysis is variability unware -> therefor we use every potential scope
        if (scopes.isEmpty) List(SCOPE_LOCAL) else scopes
    }

    private[informationflow] def genVarSource(scope: Int)(define: Id): List[SourceDefinition] = List(SourceDefinition(Variable(define), curr, scope))

    /*
     * We are using the variability-aware typesystem of TypeChef. However, variability encoded within the type definition of an variable or struct does not matter for us.
     * As a consequence we only visit one type-definition as we do assume correct type assignments.
     */
    private[informationflow] def singleVisitOnSourceTypes[T <: InformationFlowFact](currId: Id, env: succTS.VarTypingContext, structFun: (Id => List[T]), varFun: (Id => List[T])): List[T] = {
        val cTypes = env.lookupType(currId.name)
        var cFacts: List[T] = List()

        // Do not generate sources for every possible type condition; only once for either struct or variable
        if (cTypes.exists(ct => isStructOrUnion(ct)))
            cFacts :::= structFun(currId)

        if (cTypes.exists(ct => !isStructOrUnion(ct) && !isUnknownType(ct)))
            cFacts :::= varFun(currId)

        cFacts
    }

    private def genFieldSource(field: Id, parents: List[Id]): List[SourceDefinition] = {
        val scope = 1

        def findFieldType(currentType: Opt[CType], parents: List[Id]): Iterable[SourceDefinition] =
            currentType.entry.atype match {
                case cs: CStruct =>
                    val csFields = succVarEnv.structEnv.getFields(cs.s, cs.isUnion).toOptList
                    csFields.flatMap(ct => genFieldSourceWithTypeMap(ct.entry, parents.tail))
                case as: CAnonymousStruct =>
                    genFieldSourceWithTypeMap(as.fields, parents.tail)
                case _ => None
            }


        def genFieldSourceWithTypeMap(ct: ConditionalTypeMap, parents: List[Id]): List[SourceDefinition] =
            if (parents.isEmpty) {
                ct.apply(field.name).toOptList.flatMap(x => x.entry match {
                    case ct: CType if isStructOrUnion(ct) => genStructSource(scope)(field)
                    case ct: CType if !isStructOrUnion(ct) && !isUnknownType(ct) => genVarSource(scope)(field)
                    case _ => None
                })
            } else ct.apply(parents.head.name).toOptList.flatMap(findFieldType(_, parents))

        val structTypes = succVarEnv.varEnv.lookupType(parents.headOption.getOrElse(Id("_")).name).toOptList
        val result = structTypes.flatMap(findFieldType(_, parents))
        result
    }
}