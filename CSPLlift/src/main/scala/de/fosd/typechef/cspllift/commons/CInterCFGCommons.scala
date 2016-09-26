package de.fosd.typechef.cspllift.commons

import java.io.{FileOutputStream, PrintWriter}
import java.util
import java.util.zip.GZIPOutputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.customization.crewrite.AssignDeclDefUse
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c.{DeclParameterDeclList, ParameterDeclarationD, _}

import scala.collection.JavaConverters._

trait CInterCFGCommons extends AssignDeclDefUse with ASTNavigation with ConditionalNavigation with TUnitRewriteEngine {

    def writeStringToGZipFile(output : String, destination : String) = {
        val fw = new PrintWriter(new GZIPOutputStream(new FileOutputStream(destination + ".gz")))
        fw.write(output)
        fw.close()
    }

    def getFileName(originalFilePath: Option[String]): Option[String] =
        originalFilePath match {
            case None => None
            case Some(path) => Some(getPlainFileNameS(path))
        }

    def getPlainFileName(ast: AST, default: String = "NOFILENAME_AST"): String = getPlainFileNameS(ast.getFile.getOrElse(default))

    def getPlainFileNameS(str: String, default: String = "NOFILENAME"): String = {
        val regex = """^(([^/]+/)*)(([^/.]+)\..+)""".r
        val filePrefix = "file "
        str match {
            case regex(m1, m2, m3, m4) => if (m4.startsWith(filePrefix)) m4.substring(filePrefix.length) else m4
            case _ => default
        }
    }

    /*
     * Creates a java "IdentitySet" as normal java set implementation would remove equal but not identical objects like return statements
     */
    def asJavaIdentitySet[T](c: Seq[_ <: T]): java.util.Set[T] = asJavaIdentitySet(c.asJava)
    def asJavaIdentitySet[T](c: util.Collection[_ <: T]): java.util.Set[T] = {
        val res = java.util.Collections.newSetFromMap(new util.IdentityHashMap[T, java.lang.Boolean](c.size))
        res.addAll(c)
        res
    }

    def groupOptListVAware[T](l: List[Opt[T]], fm: FeatureModel): List[List[Opt[T]]] = {
        if (l.isEmpty) return List()

        var group: List[Opt[T]] = List(l.head)
        var groups: List[List[Opt[T]]] = List()
        var combinedCondition: FeatureExpr = l.head.condition

        for (curr <- l.drop(1)) {
            val groupCondition = combinedCondition.and(curr.condition)

            if (groupCondition.isContradiction(fm)) group = curr :: group
            else if (combinedCondition.implies(curr.condition).isTautology(fm)) {
                groups = group.reverse :: groups
                group = List(curr)
            }
            else group = curr :: group

            // add current feature expression as it might influence the addition of selem for
            // the remaining elements of l
            combinedCondition = combinedCondition.or(curr.condition)
        }

        (group.reverse :: groups).reverse
    }

    def getFDefParameters(fDef: Opt[FunctionDef]): List[Opt[ParameterDeclarationD]] = getFDefParameters(fDef.entry)
    def getFDefParameters(fDef: FunctionDef): List[Opt[ParameterDeclarationD]] =
        fDef.declarator.extensions.flatMap {
            case Opt(_, DeclParameterDeclList(l)) =>
                l.headOption match {
                    case Some(Opt(_ ,p: ParameterDeclarationD)) => l.asInstanceOf[List[Opt[ParameterDeclarationD]]]
                    case _ => None
                }
            case _ => None
        }

    def getPointerFDefParamNames(fDef: Opt[FunctionDef]): List[Opt[Id]] = getPointerFDefParamNames(fDef.entry)
    def getPointerFDefParamNames(fDef: FunctionDef): List[Opt[Id]] = {
        def andAll(a: FeatureExpr, b: Opt[_]): FeatureExpr = a.and(b.condition)

        getFDefParameters(fDef).flatMap(param => {
            if (param.entry.decl.pointers.nonEmpty)
                Some(Opt(param.entry.decl.pointers.foldLeft(FeatureExprFactory.True)(andAll), param.entry.decl.getId))
            else None
        })
    }

    // checks reference equality of e in a given structure t (either product or list)
    def isPartOfTerm(subterm: Product, term: Any): Boolean = {
        term match {
            case _: Product if subterm.asInstanceOf[AnyRef].eq(term.asInstanceOf[AnyRef]) => true
            case l: List[_] => l.exists(isPartOfTerm(subterm, _))
            case p: Product => p.productIterator.toList.exists(isPartOfTerm(subterm, _))
            case _ => false
        }
    }
}
