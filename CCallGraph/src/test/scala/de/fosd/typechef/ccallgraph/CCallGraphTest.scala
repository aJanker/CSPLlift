package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.c._
import org.junit.Assert.fail
import org.junit.Test


/**
 * Created by gferreir on 11/16/14.
 */
class CCallGraphTest {

    @Test def test_insert_node() {
        val callGraph : CCallGraph = new CCallGraph()
        //callGraph.apply("p", "*p", "*")

        println(callGraph.callGraph.nodes)
        println(callGraph.callGraph.edges)

    }
}
