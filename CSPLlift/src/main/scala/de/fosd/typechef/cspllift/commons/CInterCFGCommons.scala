package de.fosd.typechef.cspllift.commons

import java.io.{File, FileOutputStream, PrintWriter}
import java.util
import java.util.zip.GZIPOutputStream

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.cspllift.cintercfg.CICFGFDef
import de.fosd.typechef.customization.crewrite.AssignDeclDefUse
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c.{DeclParameterDeclList, ParameterDeclarationD, _}

import scala.collection.JavaConverters._

trait CInterCFGCommons extends AssignDeclDefUse with ASTNavigation with ConditionalNavigation with RewriteEngine {

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
        if (str.equalsIgnoreCase(default)) return default

        val filePrefix = "file "
        val prefixFree = if (str.startsWith(filePrefix)) str.substring(filePrefix.length) else str
        val index = prefixFree.lastIndexOf(File.separatorChar)
        val fName = if (index != -1) prefixFree.substring(index + 1) else prefixFree
        val extensionIndex = fName.lastIndexOf('.')
        val res = if (extensionIndex != -1) fName.substring(0, extensionIndex) else fName

        res
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

    def getCalleeParameters(fDef: CICFGFDef): List[Opt[ParameterDeclarationD]] = getCalleeParameters(fDef.method)

    def getCalleeParameters(fDef: Opt[FunctionDef]): List[Opt[ParameterDeclarationD]] = getCalleeParameters(fDef.entry)

    def getCalleeParameters(fDef: FunctionDef): List[Opt[ParameterDeclarationD]] =
        fDef.declarator.extensions.flatMap {
            case Opt(_, DeclParameterDeclList(l)) =>
                l.headOption match {
                    case Some(Opt(_ ,p: ParameterDeclarationD)) => l.asInstanceOf[List[Opt[ParameterDeclarationD]]]
                    case _ => None
                }
            case _ => None
        }

    def getPointerFDefParamNames(fDef: CICFGFDef): List[Opt[Id]] = getPointerFDefParamNames(fDef.method)
    def getPointerFDefParamNames(fDef: Opt[FunctionDef]): List[Opt[Id]] = getPointerFDefParamNames(fDef.entry)
    def getPointerFDefParamNames(fDef: FunctionDef): List[Opt[Id]] = {
        def andAll(a: FeatureExpr, b: Opt[_]): FeatureExpr = a.and(b.condition)

        getCalleeParameters(fDef).flatMap(param => {
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
