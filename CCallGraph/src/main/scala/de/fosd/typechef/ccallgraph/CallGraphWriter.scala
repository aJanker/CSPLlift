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

    /**
     *
     *
     * output format in CSV
     *
     * we distinguish nodes and edges, nodes start with "N" edges with "E"
     *
     * nodes have the following format:
     *
     * N;id;kind;line;name[::container];featureexpr;container
     *
     * * id is an identifier that only has a meaning within a file and that is not stable over multiple runs
     *
     * * kind is one of "function|function-inline|function-static|declaration|statement|expression|unknown"
     *   functions are distinguished into functions with an inline or a static modifier (inline takes precedence)
     *
     * * line refers to the starting position in the .pi file
     *
     * * name is either the name of a function or some debug information together with the name of the containing function.
     *   For functions and declarations, the name is used as a reference and can be used to match nodes across files.
     *   For expressions and statements, the name is used for debugging only and returns the first characters of the statement.
     *   In this case, the name is however followed by :: and the function name that can be used to extract hierarchy information.
     *   Note that function names should be unique in the entire system for each configuration (that is, there may be multiple
     *   functions with the same name but mutually exclusive feature expressions)
     *
     * * featureexpr describes the condition when the node is included
     *
     *
     *
     * edges do not have a line and title:
     *
     * E;sourceid;targetid;featureexpr
     *
     * they connect nodes within a file
     * ids refer to node ids within the file
     * nodeids are always declared before edges connecting them
     *
     * edges between files are not described in the output, but must be computed separately with an external linker
     * that matches nodes based on function/declaration names
     */

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
