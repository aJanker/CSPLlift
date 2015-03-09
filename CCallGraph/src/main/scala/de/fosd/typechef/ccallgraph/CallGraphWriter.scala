package de.fosd.typechef.ccallgraph

import java.io.Writer

import de.fosd.typechef.crewrite.IOUtilities
import de.fosd.typechef.featureexpr.FeatureExpr

/**
 * Created by gferreir on 1/30/15.
 */

trait GraphWriter {

    def writeNode(name: String, kind: String, sourceCodeLine: Int, fExpr : FeatureExpr)

    def writeEdge(source : String, target : String, edgeType: String, fExpr : FeatureExpr)

    def close()

}

class CallGraphWriter(fwriter: Writer) extends IOUtilities with GraphWriter {

    /**
     * output format in CSV
     *
     * we distinguish nodes and edges, nodes start with "N" edges with "E"
     *
     * nodes have the following format:
     *
     * N;id;kind;line;name[::container];featureexpr
     *
     * * id is an identifier that only has a meaning within a file and that is not stable over multiple runs
     *
     * * kind is one of "function|function-inline|function-static"
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
     * edges do not have a line and title:
     *
     * E;sourceid;targetid;featureexpr;kind
     *
     * * kind is of of D|I|FNNF|ECNF => this used to measure how precise is our pointer analysis (how many pointers are not resolved)
     *
     * they connect nodes within a file
     * ids refer to node ids within the file
     * nodeids are always declared before edges connecting them
     *
     * edges between files are not described in the output, but must be computed separately with an external linker
     * that matches nodes based on function/declaration names
     */

    override def writeNode(name : String, kind : String, sourceCodeLine : Int, fExpr: FeatureExpr): Unit = {
        fwriter.write("N;%d;%s;%d;%s;%s\n".format(System.identityHashCode(name), kind, sourceCodeLine, name, fExpr))
    }

    override def writeEdge(source: String, target: String, kind : String, fExpr: FeatureExpr): Unit = {
        fwriter.write("E;%d;%d;%s;%s\n".format(System.identityHashCode(source), System.identityHashCode(target), fExpr.toTextExpr, kind))

        // DEBUG
        // fwriter.write("E;%s;%s;%s;%s\n".format(source, target, fExpr.toTextExpr, edgeKind))
    }

    override def close() = {
        fwriter.close()
    }
}
