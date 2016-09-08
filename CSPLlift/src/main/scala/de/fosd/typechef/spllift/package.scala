package de.fosd.typechef

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.{AST, FunctionDef}
import de.fosd.typechef.spllift.cifdsproblem.CIFDSProblem
import heros.IFDSTabulationProblem


package object spllift {

    type IFDSProblem[D] = IFDSTabulationProblem[Opt[AST], D, Opt[FunctionDef], CInterCFG]

    def getCIFDSProblemInstance[D, T <: CIFDSProblem[D]](clazz: java.lang.Class[T])(args:AnyRef*): T = clazz.getConstructors()(0).newInstance(args:_*).asInstanceOf[T]

}
