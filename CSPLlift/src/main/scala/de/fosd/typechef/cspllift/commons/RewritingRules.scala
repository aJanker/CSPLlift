package de.fosd.typechef.cspllift.commons

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c._

trait RewritingRules extends ASTRewriting with ASTNavigation with ConditionalNavigation {

    def replace[T <: Product, U](t: T, e: U, n: U): T = {
        val r = manytd(rule[Any] {
            case i if i.asInstanceOf[AnyRef] eq e.asInstanceOf[AnyRef] => n
        })
        r(t).getOrElse(t).asInstanceOf[T]
    }

    def insertStmtListBeforeStmt(c: CompoundStatement, e: Opt[Statement], n: List[Opt[Statement]]): CompoundStatement = {
        val r = oncetd(rule[Any] {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef] eq e) n ::: List(x)
                    else x :: Nil)
        })
        r(c).getOrElse(c).asInstanceOf[CompoundStatement]
    }

    def replaceStmtWithStmtList[T <: Product](t: T, e: Statement, n: List[Opt[Statement]]): T = {
        val currASTEnv = CASTEnv.createASTEnv(t)
        val cc = findPriorASTElem[CompoundStatement](e, currASTEnv)

        if (cc.isEmpty) return t // statement not part of a compound statement -> can not rewrite

        val parentStmt = parentOpt(e, currASTEnv).asInstanceOf[Opt[Statement]]
        val ccReplacement = replaceStmtWithStmtsListInCCStmt(cc.get, parentStmt, n)

        replace(t, cc.get, ccReplacement)
    }

    def replaceStmtWithStmtsListInCCStmt(c: CompoundStatement, e: Opt[Statement], n: List[Opt[Statement]]): CompoundStatement = {
        val r = oncetd(rule[Any] {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef] eq e) n
                    else x :: Nil)
        })
        r(c).getOrElse(c).asInstanceOf[CompoundStatement]
    }
}