package de.fosd.typechef.cspllift.cifdsproblem.uninitializedmemory.flowfact

import de.fosd.typechef.cspllift.cifdsproblem.CDefaultFlowFact
import de.fosd.typechef.parser.c.Id

abstract class UninitializedMemoryFlowFact() extends CDefaultFlowFact {
    def GEN(i: Id): UninitializedMemoryFlowFact

    def GEN(i: Iterable[Id]): UninitializedMemoryFlowFact

    def KILL(i: Id): UninitializedMemoryFlowFact

    def KILL(i: Iterable[Id]): UninitializedMemoryFlowFact
}

case class UpdateableUninitializedMemoryFlowFact(assignedNames: Set[Id] = Set()) extends UninitializedMemoryFlowFact {

    override def GEN(i: Id): UninitializedMemoryFlowFact = UpdateableUninitializedMemoryFlowFact(assignedNames + i)

    override def GEN(i: Iterable[Id]): UninitializedMemoryFlowFact = UpdateableUninitializedMemoryFlowFact(assignedNames ++ i)

    override def KILL(i: Id): UninitializedMemoryFlowFact = UpdateableUninitializedMemoryFlowFact(assignedNames - i)

    override def KILL(i: Iterable[Id]): UninitializedMemoryFlowFact = UpdateableUninitializedMemoryFlowFact(assignedNames -- i)
}

case class Warning() extends UninitializedMemoryFlowFact {
    override def GEN(i: Id): UninitializedMemoryFlowFact = this

    override def GEN(i: Iterable[Id]): UninitializedMemoryFlowFact = this

    override def KILL(i: Id): UninitializedMemoryFlowFact = Zero()

    override def KILL(i: Iterable[Id]): UninitializedMemoryFlowFact = Zero()
}


case class Zero() extends UninitializedMemoryFlowFact {
    override def GEN(i: Id): UninitializedMemoryFlowFact = this

    override def GEN(i: Iterable[Id]): UninitializedMemoryFlowFact = this

    override def KILL(i: Id): UninitializedMemoryFlowFact = Zero()

    override def KILL(i: Iterable[Id]): UninitializedMemoryFlowFact = Zero()

}
