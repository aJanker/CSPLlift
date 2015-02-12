package de.fosd.typechef.ccallgraph

import java.io.Writer

import de.fosd.typechef.crewrite.IOUtilities
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by gferreir on 1/30/15.
 */

trait GraphWriter {

    def writeNode(value : String, fExpr : FeatureExpr)

    def writeEdge(source : String, target : String, eType: String, fExpr : FeatureExpr)

    def close()

}

class CallGraphWriter(fwriter: Writer) extends IOUtilities with GraphWriter {

    override def writeNode(value: String, fExpr: FeatureExpr): Unit = {
        fwriter.write("N;" + value +  ";" + fExpr.toTextExpr + "\n")

    }

    override def writeEdge(source: String, target: String, eType : String, fExpr: FeatureExpr): Unit = {
        fwriter.write("E;" + eType + ";" + source + ";" + target + ";" + fExpr.toTextExpr + "\n")
    }

    override def close() = {
        fwriter.close()
    }
}
