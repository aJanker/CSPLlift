package de.fosd.typechef.cmodule.cmodulecache

/**
  * Trait for register/unregister at the mediatior pattern.
  */
trait CModuleCacheMediation[T] {

    /**
      * Register a user at the mediator.
      */
    def register(user: CModuleCacheUser[T]): Unit

    /**
      * Unregister a user at the mediator.
      */
    def unregister(user: CModuleCacheUser[T]): Unit
}

/**
  * MediatorPattern for ModuleCache updates.
  */
trait CModuleCacheMediator[T] extends CModuleCacheMediation[T] {

    private var listeners = Set[CModuleCacheUser[T]]()

    override def register(user: CModuleCacheUser[T]): Unit = {
        listeners += user
        user.mediation(this, initialMediation())
    }

    override def unregister(user: CModuleCacheUser[T]): Unit = listeners -= user

    protected def initialMediation(): Iterable[T]

    protected def mediate(t: Iterable[T]): Unit = listeners.foreach(_.mediation(this, t))
}

trait CModuleCacheUser[T] {
    def mediation(cModuleEnv: CModuleCacheMediation[T], t: Iterable[T]): Boolean
}
