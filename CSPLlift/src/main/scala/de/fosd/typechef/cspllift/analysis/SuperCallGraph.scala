package de.fosd.typechef.cspllift.analysis

import de.fosd.typechef.parser.c.{AST, FunctionDef}

object SuperCallGraph {

    private lazy val nodes = new collection.mutable.HashSet[Node[FunctionDef]]()
    private lazy val edges = new collection.mutable.HashSet[Edge[FunctionDef]]()

    def addEge(edge: Edge[FunctionDef]) = {
        edges.+=(edge)
        nodes.+=(edge.from, edge.to)
    }

    def write(writer: IFGWriter)  = {
        writer.writeHeader()
        edges.foreach(edge => writer.writeEdge(edge.asInstanceOf[Edge[AST]]))
        nodes.foreach(node => writer.writeNode(node.asInstanceOf[Node[AST]]))
        writer.writeFooter()
        writer.close()
    }

    def clear() = {
        nodes.clear()
        edges.clear()
    }
}
