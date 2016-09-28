package de.fosd.typechef.customization.crewrite

import de.fosd.typechef.conditional._
import de.fosd.typechef.parser.c._
import org.kiama.attribution.Attribution._

// defines functions to compute sets for used, defined, and declared variables
// used for Liveness and ReachingDefinitions
trait AssignDeclDefUse {

    // returns all declared Ids independent of their annotation
    lazy val declares: AnyRef => List[Id] =
    attr {
        case DeclarationStatement(decl) => declares(decl)
        case Declaration(_, init) => init.flatMap(declares)
        case InitDeclaratorI(declarator, _, _) => declares(declarator)
        case AtomicNamedDeclarator(_, id, _) => List(id)
        case Opt(_, entry) => declares(entry.asInstanceOf[AnyRef])
        case Some(entry) => declares(entry.asInstanceOf[AnyRef])
        case _ => List()
    }

    // returns all defined Ids independent of their annotation
    lazy val defines: AnyRef => List[Id] =
    attr {
        case AssignExpr(target: Id, _, source) => List(target)
        case DeclarationStatement(d) => defines(d)
        case Declaration(_, init) => init.flatMap(defines)
        case InitDeclaratorI(i, _, _) => defines(i)
        case AtomicNamedDeclarator(_, i, _) => List(i)
        case ExprStatement(_: Id) => List()
        case ExprStatement(PointerDerefExpr(_)) => List()
        case ExprStatement(expr) => defines(expr)
        case ExprList(exprs) => exprs.flatMap(defines)
        case PointerDerefExpr(i: Id) => List(i)
        case Opt(_, entry) => defines(entry.asInstanceOf[AnyRef])
        case Some(entry) => defines(entry.asInstanceOf[AnyRef])
        /* //TODO Removal?
          case PostfixExpr(i@Id(_), SimplePostfixSuffix(_)) => List(i) // a++; or a--;
          case UnaryExpr(kind, i: Id) => if (kind == "++" || kind == "--") List(i) else List() // ++a; or --a;
        */
        case _ => List()
    }


    // returns all used Ids independent of their annotation
    lazy val uses: AnyRef => List[Id] =
    attr {
        case ForStatement(expr1, expr2, expr3, _) => uses(expr1) ++ uses(expr2) ++ uses(expr3)
        case ReturnStatement(Some(x)) => uses(x)
        case WhileStatement(expr, _) => uses(expr)
        case DeclarationStatement(d) => uses(d)
        case Declaration(_, init) => init.flatMap(uses)
        case InitDeclaratorI(_, _, Some(i)) => uses(i)
        case AtomicNamedDeclarator(_, id, _) => List(id)
        case NestedNamedDeclarator(_, nestedDecl, _, _) => uses(nestedDecl)
        case Initializer(_, expr) => uses(expr)
        case i: Id => List(i)
        case FunctionCall(params) => params.exprs.map(_.entry).flatMap(uses)
        case ArrayAccess(expr) => uses(expr)
        case PostfixExpr(_: Id, f: FunctionCall) => uses(f)
        case PostfixExpr(p, s) => uses(p) ++ uses(s)
        case UnaryExpr(_, ex) => uses(ex)
        case SizeOfExprU(expr) => uses(expr)
        case CastExpr(_, expr) => uses(expr)
        case PointerDerefExpr(castExpr) => uses(castExpr)
        case PointerCreationExpr(castExpr) => uses(castExpr)
        case UnaryOpExpr(kind, castExpr) => uses(castExpr)
        case NAryExpr(ex, others) => uses(ex) ++ others.flatMap(uses)
        case NArySubExpr(_, ex) => uses(ex)
        case ConditionalExpr(condition, _, _) => uses(condition)
        case ExprStatement(expr) => uses(expr)
        case ExprList(exprs) => exprs.flatMap(uses)
        case AssignExpr(target, op, source) => uses(source) ++ (if (op == "=") List() else uses(target))
        case Opt(_, entry) => uses(entry.asInstanceOf[AnyRef])
        case Some(entry) => uses(entry.asInstanceOf[AnyRef])
        case ParameterDeclarationD(_, decl, _) => uses(decl)
        case l: List[AnyRef@unchecked] => l.flatMap(uses)
        case x => List()

    }


    // Returns all the parent structs of an field
    private val parents: AnyRef => List[Id] =
    attr {
        case PostfixExpr(p, s) => parents(p) ++ parents(s)
        case PointerPostfixSuffix(_, i) => List(i)
        case i: Id => List(i)
        case _ => List()
    }

    /**
      * Returns all defined fields and its struct parents
      * e.g. x.y.z = foo => List(Id(z), List(Id(x), Id(y))
      */
    lazy val assignsField: AnyRef => List[(Id, List[Id])] =
    attr {
        case AssignExpr(p: PostfixExpr, _, _) => assignsField(p)
        case DeclarationStatement(d) => assignsField(d)
        case Declaration(_, init) => init.flatMap(assignsField)
        case InitDeclaratorI(i, _, _) => assignsField(i)
        case ExprStatement(_: Id) => List()
        case ExprStatement(PointerDerefExpr(_)) => List() // TODO Struct Pointer
        case ExprStatement(expr) => assignsField(expr)
        case ExprList(exprs) => exprs.flatMap(assignsField)
        case Opt(_, entry) => assignsField(entry.asInstanceOf[AnyRef])
        case Some(entry) => assignsField(entry.asInstanceOf[AnyRef])
        case PostfixExpr(p, PointerPostfixSuffix(_, i: Id)) => List((i, parents(p)))
        case _ => List()
    }

    /**
      * Returns all used fields and its struct parents
      * e.g. foo = x.y.z => List(Id(z), List(Id(x), Id(y))
      */
    lazy val usesField: AnyRef => List[(Id, List[Id])] =
    attr {
        case ForStatement(expr1, expr2, expr3, _) => usesField(expr1) ++ usesField(expr2) ++ usesField(expr3)
        case ReturnStatement(Some(x)) => usesField(x)
        case WhileStatement(expr, _) => usesField(expr)
        case DeclarationStatement(d) => usesField(d)
        case Declaration(_, init) => init.flatMap(usesField)
        case InitDeclaratorI(_, _, Some(i)) => usesField(i)
        case NestedNamedDeclarator(_, nestedDecl, _, _) => usesField(nestedDecl)
        case Initializer(_, expr) => usesField(expr)
        case FunctionCall(params) => params.exprs.map(_.entry).flatMap(usesField)
        case PostfixExpr(_: Id, f: FunctionCall) => usesField(f)
        case PostfixExpr(p, PointerPostfixSuffix(_, i: Id)) => List((i, parents(p)))
        case PostfixExpr(p, _) => usesField(p)
        case PointerDerefExpr(p) => usesField(p)
        case UnaryExpr(_, ex) => usesField(ex)
        case SizeOfExprU(expr) => usesField(expr)
        case CastExpr(_, expr) => usesField(expr)
        case UnaryOpExpr(kind, castExpr) => usesField(castExpr)
        case NAryExpr(ex, others) => usesField(ex) ++ others.flatMap(usesField)
        case NArySubExpr(_, ex) => usesField(ex)
        case ConditionalExpr(condition, _, _) => usesField(condition)
        case ExprStatement(expr) => usesField(expr)
        case ExprList(exprs) => exprs.flatMap(usesField)
        case AssignExpr(target, op, source) => usesField(source) ++ (if (op == "=") List() else usesField(target))
        case Opt(_, entry) => usesField(entry.asInstanceOf[AnyRef])
        case Some(entry) => usesField(entry.asInstanceOf[AnyRef])
        case i: Id => List()
        case i: InitDeclaratorI => List()
        case c: Constant => List()
        case _ => List()
    }

    lazy val assignsVariables: AnyRef => List[(Id, List[Id])] =
        attr {
            case AssignExpr(PostfixExpr(target: Id, _), _, source) => if (uses(source).nonEmpty) List((target, uses(source))) else List((target, List()))
            case AssignExpr(target: Id, _, source) => if (uses(source).nonEmpty) List((target, uses(source))) else List()
            case DeclarationStatement(d) => assignsVariables(d)
            case Declaration(_, init) => init.flatMap(assignsVariables)
            case InitDeclaratorI(i, _, init) if init.isDefined => if (uses(init).nonEmpty) List((i.getId, uses(init))) else List()
            case ExprStatement(expr) => assignsVariables(expr)
            case ExprList(exprs) => exprs.flatMap(assignsVariables)
            case Opt(_, entry) => assignsVariables(entry.asInstanceOf[AnyRef])
            case Some(entry) => assignsVariables(entry.asInstanceOf[AnyRef])
            case x => List()
        }
}
