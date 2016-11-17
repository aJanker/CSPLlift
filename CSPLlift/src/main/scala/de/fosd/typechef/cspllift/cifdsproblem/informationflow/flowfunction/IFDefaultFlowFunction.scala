package de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfunction

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.sinkorsource._
import de.fosd.typechef.cspllift.cifdsproblem.informationflow.flowfact.{InformationFlowFact, Zero}
import de.fosd.typechef.cspllift.cintercfg.{CICFGNode, CInterCFG}
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory
import de.fosd.typechef.parser.c.{AST, FunctionDef, Id}
import de.fosd.typechef.typesystem.{CAnonymousStruct, CStruct, CType, ConditionalTypeMap}
import org.slf4j.{Logger, LoggerFactory}


abstract class IFDefaultFlowFunction(interproceduralCFG: CInterCFG, curr: CICFGNode, succ: CICFGNode) extends IFFlowFunction {
    private lazy val logger: Logger = LoggerFactory.getLogger(getClass)

    lazy val currStmt = curr.getStmt
    lazy val succStmt = succ.getStmt

    // Lazy caching of some repeatedly used variables
    lazy val currASTEnv = interproceduralCFG.getASTEnv(curr)
    lazy val currOpt: Opt[AST] = parentOpt(curr.getStmt.entry, currASTEnv).asInstanceOf[Opt[AST]]
    lazy val currTS = interproceduralCFG.getTS(curr)

    lazy val succTS = interproceduralCFG.getTS(succ)
    lazy val succVarEnv = succ.getStmt.entry match {
        case f: FunctionDef => succTS.lookupEnv(f.stmt.innerStatements.headOption.getOrElse(currOpt).entry)
        case _ => succTS.lookupEnv(succ.getStmt.entry)
    }

    lazy val currDefines = defines(currStmt)
    lazy val currUses = uses(currStmt)
    lazy val currAssignments = assignsVariables(currStmt)
    lazy val currStructFieldAssigns = assignsField(currStmt)
    lazy val currStructFieldUses = usesField(currStmt)
    lazy val currStatementIsAssignment: Boolean = currDefines.nonEmpty || currAssignments.nonEmpty || currStructFieldAssigns.nonEmpty
    lazy val currSatisfiableCondition = currSatisfiableAssignment.getOrElse(defaultSatisfiableAssignment)._1.map(_.feature).toSet
    lazy val defineSources = currDefines.flatMap(genSource)
    lazy val assignmentSources =
        if (currStructFieldAssigns.isEmpty) currAssignments.flatMap { case (assignee, _) => genSource(assignee) }
        else currStructFieldAssigns.flatMap { case (field, parents) => genSourceForField(field, parents, getCurrentScope(parents.lastOption.getOrElse(field))) }
    private lazy val defaultSatisfiableAssignment = (BDDFeatureExprFactory.TrueB.collectDistinctFeatureObjects, BDDFeatureExprFactory.FalseB.collectDistinctFeatureObjects)
    private lazy val currSatisfiableAssignment = currASTEnv.featureExpr(currStmt.entry).getSatisfiableAssignment(interproceduralCFG.getFeatureModel, currOpt.condition.collectDistinctFeatureObjects, preferDisabledFeatures = false)

    override def computeSink(s: Sink): util.Set[InformationFlowFact] = if (interproceduralCFG.isStartPoint(curr)) GEN(s) else KILL

    override def computeZero(z: Zero): util.Set[InformationFlowFact] = GEN(z)

    def getDefineSourcesFromAssignment(define: Id): List[SourceDefinition] = assignmentSources.filter(_.getType.getName.equals(define))

    def currFactIsAssignee(fact: Source): Boolean =
        fact match {
            case s: Source if (s.getType match {
                case Struct(_, Some(_)) => true
                case _ => false
            }) => currStructFieldAssigns.exists(assignment => isFullFieldMatch(fact, assignment) || isPartFieldMatch(fact, assignment))
            case s: Source => currDefines.exists(s.getType.getName.equals) || currAssignments.exists { case (assignee, _) => s.getType.getName.equals(assignee) }
            case _ => false
        }

    def getSinksAndSourcesOf(currSourceDefinition: SourceDefinition, sources: List[SourceDefinition]): List[InformationFlowFact] = {
        val sourcesOf = sources.flatMap {
            case s: Source => Some(SourceDefinitionOf(s.getType, curr, currSourceDefinition, s.getScope))
            case _ => None
        }

        val sinks = sources.map(genSource => SinkToAssignment(curr, currSourceDefinition, genSource.getType.getName))

        sourcesOf ::: sinks
    }

    def genSourceForField(field: Id, parents: List[Id], scopes: List[Int]): List[SourceDefinition] = {
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

    protected def genStructSource(scope: Int)(define: Id): List[SourceDefinition] = List(SourceDefinition(Struct(define, None), curr, scope))

    private def genSource(define: Id): List[SourceDefinition] = {
        val scopes = getCurrentScope(define)
        scopes.flatMap(scope => singleVisitOnSourceTypes(define, succVarEnv.varEnv, genStructSource(scope), genVarSource(scope)))
    }

    def getCurrentScope(id: Id): List[Int] = {
        val scopes = succVarEnv.varEnv.lookupScope(id.name).toOptList.map(_.entry).filter(!_.equals(SCOPE_UNKNOWN)) // our analysis is variability unware -> therefor we use every potential scope
        if (scopes.isEmpty) List(SCOPE_LOCAL) else scopes
    }

    protected def genVarSource(scope: Int)(define: Id): List[SourceDefinition] = List(SourceDefinition(Variable(define), curr, scope))

    /*
     * We are using the variability-aware typesystem of TypeChef. However, variability encoded within the type definition of an variable or struct does not matter for us.
     * As a consequence we only visit one type-definition as we do assume correct type assignments.
     */
    protected def singleVisitOnSourceTypes[T <: InformationFlowFact](currId: Id, env: succTS.VarTypingContext, structFun: (Id => List[T]), varFun: (Id => List[T])): List[T] = {
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