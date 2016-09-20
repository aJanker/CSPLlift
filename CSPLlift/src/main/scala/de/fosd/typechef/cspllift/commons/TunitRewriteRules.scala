package de.fosd.typechef.cspllift.commons

import de.fosd.typechef.conditional.Opt
import org.kiama.rewriting.Rewriter._

trait TunitRewriteRules {

    def replace[T <: Product, U](t: T, e: U, n: U): T = {
        val r = manybu(rule[Any] {
            case i if i.asInstanceOf[AnyRef] eq e.asInstanceOf[AnyRef] => n
        })
        r(t).getOrElse(t).asInstanceOf[T]
    }

    def replace[T <: Product](t: T, mark: Opt[_], replace: Opt[_]): T = {
        val r = manybu(rule[Any] {
            case l: List[_] => l.flatMap(x =>
                if (x.asInstanceOf[AnyRef].eq(mark))
                    replace :: Nil
                else
                    x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

}
