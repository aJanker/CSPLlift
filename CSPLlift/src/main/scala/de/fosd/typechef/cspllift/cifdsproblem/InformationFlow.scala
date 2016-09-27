package de.fosd.typechef.cspllift.cifdsproblem

import java.io.{StringWriter, Writer}

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.commons.KiamaRewritingRules
import de.fosd.typechef.cspllift.evaluation.SimpleConfiguration
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExprFactory
import de.fosd.typechef.parser.c._

import scala.collection.mutable.ListBuffer


sealed trait InformationFlow extends Product with Cloneable with CFlowFact with KiamaRewritingRules {
    override def clone(): InformationFlow.this.type = super.clone().asInstanceOf[InformationFlow.this.type]
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = false
    override def isInterestingFact: Boolean = false
    override def toText: String = toString

    override def get: CFlowFact = this
}


case class Zero(override val flowCondition : FeatureExpr = BDDFeatureExprFactory.TrueB) extends InformationFlow with CZeroFact {
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = other.isInstanceOf[Zero]
}

sealed abstract class Source(val name: Opt[Id], val stmt: Opt[_], val reachingSources: ListBuffer[Source] = ListBuffer(), val globalFile: Option[String] = None) extends InformationFlow {
    override def hashCode() = name.hashCode + stmt.hashCode() + globalFile.hashCode()
}

case class VarSource(override val name: Opt[Id], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def equals(other: Any) = other match {
        case s@VarSource(oName, oStmt, oReachingSources, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile) && oReachingSources.diff(reachingSources).isEmpty
        case _ => false
    }
}

case class StructSource(override val name: Opt[Id], field: Option[Source], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def equals(other: Any) = other match {
        case s@StructSource(oName, _, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
        case _ => false
    }
}

// TODO to implement
case class PointerSource(override val name: Opt[Id], override val stmt: Opt[_], override val reachingSources: ListBuffer[Source] = ListBuffer(), override val globalFile: Option[String] = None) extends Source(name, stmt, reachingSources, globalFile) {
    override def equals(other: Any) = other match {
        case s@PointerSource(oName, oStmt, _, oGlobalFile) => oName.equals(name) && oStmt.equals(stmt) && oGlobalFile.equals(globalFile)
        case _ => false
    }
}


case class Reach(to: Opt[AST], from: List[Opt[Id]], sources: List[Source]) extends InformationFlow {
    override def isInterestingFact: Boolean = true

    // TODO Refactor
    override def isEquivalentTo(other: CFlowFact, configuration: SimpleConfiguration): Boolean = {
        if (!other.isInstanceOf[Reach])
            return false

        val otherReach = other.asInstanceOf[Reach]

        val toProduct = deriveProductWithCondition(to.entry, configuration.getTrueFeatures)
        val eqTo = toProduct.equals(otherReach.to.entry)
        val fromEntry = from.map(_.entry)
        val otherFromEntry = otherReach.from.map(_.entry)

        eqTo && fromEntry.equals(otherFromEntry)
    }


    override def toText: String = toText(new StringWriter).toString

    def toText(writer: Writer) : Writer = {
        writer.append("Reach under condition " + to.condition.toTextExpr + " at " + PrettyPrinter.print(CompoundStatement(List(to.asInstanceOf[Opt[Statement]]))) + "\n")

        if (from.nonEmpty) {
            writer.append("\tFrom:\t")
            from.foreach(entry => writer.append("\n\t" + entry.entry.name + " (" + entry.entry.getPositionFrom + ") when " + entry.condition.toTextExpr + ";"))
        }

        //if (sources.nonEmpty) writer.append("\n\tSources: " + sources)

        writer
    }
}

