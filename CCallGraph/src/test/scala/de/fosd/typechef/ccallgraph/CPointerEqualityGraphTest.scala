package de.fosd.typechef.ccallgraph

import java.io.{FileNotFoundException, InputStream}

import de.fosd.typechef.parser.c._
import org.junit.Test

/**
 * Created by gferreir on 9/26/14.
 */
class CPointerEqualityGraphTest extends TestHelper with ASTNavigation {

    private def loadAST(filename : String) : AST = {
        val folder = "testfiles/"
        val instream: InputStream = getClass.getResourceAsStream("/" + folder + filename)
        if (instream == null)
            throw new FileNotFoundException("Input file not found!")
        val ast = parseFile(instream, folder, filename)
        ast
    }

    private def extractObjectNames[T <: AST](ast: AST) = {
        val env = CASTEnv.createASTEnv(ast)
        val fdefs = env.keys()
        for (fdef <- fdefs) {
            println(fdef)
        }
    }

    @Test def extract_object_names() {
        val ast = loadAST("simple_variable_assignments.c")
        extractObjectNames(ast)
    }
}
