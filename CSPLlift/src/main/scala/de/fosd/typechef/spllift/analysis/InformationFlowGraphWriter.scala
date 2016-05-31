package de.fosd.typechef.spllift.analysis

import java.io.Writer

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{Declaration, Expr, _}

case class Node(value: Opt[AST]) {
    override def equals(that: Any) = that match {
        case that: Node => (that canEqual this) && (value equals that.value) && value.entry.getPositionFrom.equals(that.value.entry.getPositionFrom)
        case _ => false
    }
    override def canEqual(that: Any) = that.isInstanceOf[Node]
    override def hashCode = value.hashCode + value.entry.getPositionFrom.hashCode
}

case class Edge(from: Node, to: Node, condition: FeatureExpr) {
    override def equals(that: Any) = that match {
        case that: Edge => (that canEqual this) && ((this.from equals that.from) && (this.to equals that.to) && this.condition == that.condition)
        case _ => false
    }
    override def canEqual(that: Any) = that.isInstanceOf[Edge]
    override def hashCode = from.hashCode + to.hashCode + condition.hashCode
}

trait IFGWriter {

    val normalNodeFontName = "Calibri"

    val normalNodeFontColor = "black"

    val normalNodeFillColor = "white"

    val externalDefNodeFontColor = "blue"

    val featureNodeFillColor = "#CD5200"

    val normalConnectionEdgeColor = "black"

    val normalConnectionEdgeThickness = "setlinewidth(1)"

    val featureConnectionEdgeColor = "red"

    val textLength = 50

    def writeHeader(header: String = "")

    def writeFooter()

    def writeNode(node: Node)

    def writeEdge(edge: Edge)

    def close()
}

class InformationFlowGraphWriter(writer: Writer) extends IFGWriter {

    override def close() = writer.close()

    override def writeHeader(title: String = "") = {
        writer.write("digraph \"" + title + "\" {" + "\n")
        writer.write("node [shape=record];\n")
    }

    override def writeFooter() = writer.write("}\n")

    override def writeNode(node: Node) = {
        val ast = node.value.entry
        val condition = node.value.condition

        val op = esc(asText(ast))
        writer.write("\"" + node.hashCode + "\"")
        writer.write("[")

        val position =
            if (ast.hasPosition) ast.getPositionFrom.toString
            else "No Position available"

        writer.write("label=\"{{" + op + "}|" + "{" + position + "}|" + esc(condition.toString) + "}\", ")

        writer.write("color=\"" + (if (ast.isInstanceOf[ExternalDef]) externalDefNodeFontColor else normalNodeFontColor) + "\", ")
        writer.write("fontname=\"" + normalNodeFontName + "\", ")
        writer.write("style=\"filled\"" + ", ")
        writer.write("fillcolor=\"" + (if (condition.isTautology()) normalNodeFillColor else featureNodeFillColor) + "\"")

        writer.write("];\n")
    }

    override def writeEdge(edge: Edge) = {
        val from = edge.from
        val to = edge.to
        val condition = edge.condition
        writer.write("\"" + from.hashCode + "\" -> \"" + to.hashCode + "\"")
        writer.write("[")
        writer.write("label=\"" + condition.toTextExpr + "\", ")
        writer.write("color=\"" + (if (condition.isTautology()) normalConnectionEdgeColor else featureConnectionEdgeColor) + "\", ")
        writer.write("style=\"" + normalConnectionEdgeThickness + "\"")
        writer.write("];\n")
    }

    private def esc(i: String) = {
        i.replace("\n", "\\l").
            replace("{", "\\{").
            replace("}", "\\}").
            replace("<", "\\<").
            replace(">", "\\>").
            replace("\"", "\\\"").
            replace("|", "\\|").
            replace(" ", "\\ ").
            replace("\\\"", "\\\\\"").
            replace("\\\\\"", "\\\\\\\"").
            replace("\\\\\\\\\"", "\\\\\\\"")
    }

    private def asText(o: AST): String = o match {
        case FunctionDef(_, decl, _, _) => "Function " + o.getPositionFrom.getLine + ": " + decl.getName
        case s: Statement => "Stmt " + s.getPositionFrom.getLine + ": " + PrettyPrinter.print(s).take(textLength)
        case e: Expr => "Expr " + e.getPositionFrom.getLine + ": " + PrettyPrinter.print(e).take(textLength)
        case Declaration(_, initDecl) => "Decl " + o.getPositionFrom.getLine + ": " + initDecl.map(_.entry.getName).mkString(", ")
        case x => esc(PrettyPrinter.print(x)).take(textLength)
    }
}
