// https://stackoverflow.com/questions/33062795/find-name-of-lambda-argument-passed-in-scala
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import scala.reflect.runtime.universe

sealed trait Ast

object Ast {
  case class Ident(name: String) extends Ast

  case class Equal(left: Ast, right: Ast) extends Ast
  case class Unequal(left: Ast, right: Ast) extends Ast

  case class And(left: Ast, right: Ast) extends Ast
  case class Or(left: Ast, right: Ast) extends Ast

  case class Plus(left: Ast, right: Ast) extends Ast

  case class Field(target: String, name: String) extends Ast
  case class Method(ast: Ast, name: String, args: Seq[Ast]) extends Ast

  case class Raw(obj: Any) extends Ast

  case class In(target: Ast, list: Ast) extends Ast

  case class If(cond: Ast, left: Ast, right: Ast) extends Ast

  case class Match(expr: Ast, cases: Seq[Ast]) extends Ast
  case class CaseDef(matcher: Ast, cond: Ast, expr: Ast) extends Ast
}

object MyFilter {
  def filter[T](p: T => Boolean): Ast = macro filterImpl[T]

  private def isPure(ast: Ast): Boolean = {
    ast match {
      case _: Ast.Raw => true
      case _ => false
    }
  }

  private def lnr: Int = {
    (new Exception).getStackTrace.toSeq(1).getLineNumber
  }

  private def log(any: Any = null): Unit = {
    val lnr = (new Exception).getStackTrace.toSeq(1).getLineNumber

    val text = any match {
      case null => ""
      case o => o.toString
    }
    println(s"$lnr: $text")
  }


  def filterImpl[T](c: Context)(p: c.Expr[T => Boolean])(implicit tg: c.universe.WeakTypeTag[T]): c.Expr[Ast] = {
    import c.universe._

    def show2(modifiers: Modifiers): String = {
      modifiers match {
        case Modifiers(flags, mname, annotations) =>
          s"Modifiers($flags, $mname, $annotations)"
      }
    }

    def show(tree: Tree): String = {
      tree match {
        case Literal(l) =>
          s"Literal(${l})"
        case Apply(ident, args) =>
          //println(args(0).getClass)
          val arguments = args.map(show).mkString(", ")
          s"Apply(${show(ident)}, List($arguments))"
        case Select(a, TermName(term)) =>
          s"Select(${show(a)}, TermName(${term}))"
        case Ident(name) =>
          s"Ident(${name})"
        case TypeApply(target, arguments) =>
          s"TypeApply(${show(target)}, ${arguments.map(show)})"
        case typeTree: TypeTree =>
          s"TypeTree(${typeTree})"
        case ValDef(Modifiers(flags, mname, annotations), TermName(name), tp, rhs) =>
          s"ValDef(Modifiers($flags, $mname, $annotations), $name, ${show(tp)}, ${show(rhs)})"
        case DefDef(modifiers, TermName(defName), c, argDefs, typeTree, body) =>
          val argDefsString = argDefs.map(a => a.map(show))
          s"DefDef(${show2(modifiers)}, $defName, $c, $argDefsString, $typeTree, ${show(body)})"
        case EmptyTree =>
          "EmptyTree"
        case Function(args, body) =>
          s"Function(${args.map(show)}, $body)"
        case Typed(target, typeTree) =>
          s"Typed(${show(target)}, $typeTree)"
        case If(cond, left, right) =>
          s"If(${show(cond)}, ${show(left)}, ${show(right)})"
        case Match(a, b) =>
          s"Match($a, ${b.map(show)})"
        case CaseDef(a, b, c) =>
          s"CaseDef(${show(a)}, ${show(b)}, ${show(c)})"
        case UnApply(a, b) =>
          s"UnApply($a, $b)"
        case Block(defs, expr) =>
          val defsString = defs.map(show).mkString(", ")
          s"Block(($defsString), ${show(expr)})"
        case other =>
          println(s"class: ${other}")
          println(other.getClass)
          ???
      }
    }

    // this is awesome
    //println(s"prefix: ${c.prefix}")
    //println(s"isCaseClass: ${tg.tpe.typeSymbol.asClass.isCaseClass}")
    //println(s"isCaseClass: ${tg.tpe.typeSymbol.name}")

    val `==` = TermName("$eq$eq")
    val `!=` = TermName("$bang$eq")

    val `&&` = TermName("$amp$amp")
    val `||` = TermName("$bar$bar")

    val `<=` = TermName("$less$equal")
    val `>=` = TermName("$greater$equal")
    val `+` = TermName("$plus")

    val Function(args, body) = p.tree
    val ValDef(mods, name, tp, rhs) = args(0)

    def isPureTree(tree: Tree): Boolean = tree match {
      case EmptyTree =>
        true
      case Literal(_) =>
        true
      case Ident(TermName(termName)) =>
        termName != name.toString
      case Select(tree, _) =>
        isPureTree(tree)
      case If(cond, left, right) =>
        isPureTree(cond) && isPureTree(left) && isPureTree(right)
      case Apply(target, arguments) =>
        isPureTree(target) && arguments.forall(isPureTree)
      case TypeApply(klass, typeTree) =>
        isPureTree(klass)
      case CaseDef(matcher, cond, expr) =>
        log(cond)
        isPureTree(cond) && isPureTree(expr)
      case ValDef(mods, name, tp, rhs) =>
        isPureTree(rhs)
      case Match(expr, cases) =>
        isPureTree(expr) && cases.forall(isPureTree)
      case DefDef(modifiers, defName, c, argDefs, typeTree, body) =>
        // TODO: figure out what is happening here
        // argDefs.forall(valDef => isPureTree(valDef)) &&
        isPureTree(body)
      case Block(defs, expr) =>
        defs.forall(isPureTree) && isPureTree(expr)
      case other =>
        log(s"omg ${other.getClass} $other")
        ???
    }


    def select(s: Tree, name: String, path: Seq[String] = Seq.empty): Ast = s match {
      case Ident(TermName(termName)) if termName == name =>
        log()
        Ast.Field(tg.tpe.typeSymbol.name.toString, path.reverse.mkString("."))
      case Select(s, TermName(fieldName)) =>
        log()
        select(s, name, path :+ fieldName)
      case Apply(Select(Select(Ident(TermName("scala")), TermName("Predef")), TermName("augmentString")), List(rest)) =>
        log()
        select(rest, name, path)
      case other =>
        log(other)
        Ast.Raw(other)
    }

    def pure(t: Tree): Ast = {
      val ast = convert(t)
      if (isPure(ast)) {
        Ast.Raw(t)
      } else {
        ast
      }

    }

    def convert(t: Tree): Ast = {
      t match {
        case s: Select =>
          log()
          select(s, name.toString)
        case Literal(Constant(right)) =>
          log()
          Ast.Raw(Literal(Constant(right)))
        case Apply(Select(Apply(klass, List(left)), TermName("contains")), List(right)) =>
          log()
          klass.toString match {
            case "scala.Predef.intArrayOps" =>
              Ast.In(convert(right), convert(left))
            case "scala.Predef.refArrayOps[String]" =>
              Ast.In(convert(right), convert(left))
          }

        case Apply(Select(Apply(TypeApply(klass, typeTree), List(left)), TermName("contains")), List(right)) =>
          log()
          klass.toString match {
            case "scala.Predef.refArrayOps" =>
              Ast.In(convert(right), convert(left))
          }
        case Apply(Select(l, op), List(r)) =>
          log(op)
          val leftAst = pure(l)
          val rightAst = pure(r)

          if (isPure(leftAst) && isPure(rightAst)) {
            Ast.Raw(Apply(Select(l, op), List(r)))
          } else {
            op match {
              case `==` => Ast.Equal(leftAst, rightAst)
              case `!=` => Ast.Unequal(leftAst, rightAst)

              case `&&` => Ast.And(leftAst, rightAst)
              case `||` => Ast.Or(leftAst, rightAst)
              case `+` => Ast.Plus(leftAst, rightAst)

              case TermName(methodName) =>
                if (isPure(leftAst)) {
                  Ast.Raw(Apply(Select(l, op), List(r)))
                } else {
                  Ast.Method(leftAst, methodName, Seq(rightAst))
                }
            }
          }
        case Apply(Select(l, TermName(methodName)), args) =>
          log(show(l))
          Ast.Method(convert(l), methodName, args.map(convert))
        case Apply(Ident(funcName), args) =>
          //println(s"HERE: $funcName ${funcName.getClass}")
          val arguments = args.map(convert)
          if (arguments.forall(isPure)) {
            Ast.Raw(Apply(Ident(funcName), args))
          } else {
            //Ast.Raw(Apply(Ident(funcName), args))
            //println("HERE2")
            Ast.Raw(Apply(Ident(funcName), args))

          }
        case Ident(TermName(o)) =>
          log(o)
          Ast.Raw(Ident(TermName(o)))
        case Apply(TypeApply(Select(target, TermName("contains")), List(typeTree)), List(rest)) =>
          log()
          Ast.In(convert(rest), Ast.Raw(target))
        // TODO: move this up
        case pureTree if isPureTree(pureTree) =>
          Ast.Raw(pureTree)
        case If(cond, left, right) =>
          log()
          Ast.If(pure(cond), pure(left), pure(right))
        case Block(defs, expr) =>
          log(defs)
          log(expr)
          ???
        case ValDef(mods, name, tp, rhs) =>
          ???
        case Match(expr, cases) =>
          Ast.Match(pure(expr), cases.map(pure))
        case CaseDef(matcher, cond, expr) =>
          Ast.CaseDef(pure(matcher), pure(cond), pure(expr))
          /*
        case Apply(TypeApply(name, List(typeTree)), List(target)) =>
          println(lnr)
          name.toString match {
            case "scala.Predef.refArrayOps" =>
              val t = Ast.Raw(target)
              println(s"ref array ops $typeTree: ${show(target)}")
              println(t)
              t
            case _ =>
              println(s"name: $name")
              ???

          }
          */
        //case Apply(TypeApply(Select(Select(Ident("scala"), TermName("Predef")), TermName("refArrayOps")), List(typeTree)), List(target)) =>


        case other =>
          log(s"missing: ${other.getClass} ${show(other)}")
          ???

      }
    }


    def const(value: Any): Tree = {
      Literal(Constant(value))
    }

    def convert2(ast: Ast): Tree = {
      ast match {
        case Ast.Ident(value) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Ident")), TermName("apply")), List(const(value)))

        case Ast.Equal(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Equal")), TermName("apply")), List(convert2(left), convert2(right)))
        case Ast.Unequal(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Unequal")), TermName("apply")), List(convert2(left), convert2(right)))

        case Ast.Field(target, name) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Field")), TermName("apply")), List(const(target), const(name)))

        case Ast.And(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("And")), TermName("apply")), List(convert2(left), convert2(right)))
        case Ast.Or(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Or")), TermName("apply")), List(convert2(left), convert2(right)))

        case Ast.Plus(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Plus")), TermName("apply")), List(convert2(left), convert2(right)))

        case Ast.Method(ast, methodName, args) =>
          val args2: List[Tree] = args.toList.map(a => convert2(a))
          val arguments = Apply(Select(Ident(TermName("List")), TermName("apply")), args2)
          val list = List(convert2(ast), Literal(Constant(methodName)), arguments)
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Method")), TermName("apply")), list)
        case Ast.Raw(o) =>
          val tree = o.asInstanceOf[Tree]
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Raw")), TermName("apply")), List(tree))

        case Ast.In(target, list) =>
          val t = convert2(target)
          val l = convert2(list)
          Apply(Select(Select(Ident(TermName("Ast")), TermName("In")), TermName("apply")), List(t, l))
        case Ast.If(Ast.Raw(cond: Tree), left, right) =>
          log(ast)
          If(cond, convert2(left), convert2(right))
        case Ast.If(cond, left, right) =>
          log(ast)
          val c = convert2(cond)
          val l = convert2(left)
          val r = convert2(right)
          Apply(Select(Select(Ident(TermName("Ast")), TermName("If")), TermName("apply")), List(c, l, r))
        case Ast.Match(Ast.Raw(expr), cases) =>
          ???
        case Ast.Match(expr, cases) =>
          val e = convert2(expr)
          //val c = cases.map(c => convert2(c))
          //Apply(Select(Select(Ident(TermName("Ast")), TermName("Match")), TermName("apply")), List(e, c))
          ???
      }
    }

    // scala.collection.immutable.Seq.apply[Ast.Raw](Ast.Raw.apply(1)).isEmpty
    //
    // Select(Apply(
    //   TypeApply(Select(Select(Select(Select(Ident(scala), TermName(collection)), TermName(immutable)), TermName(Seq)), TermName(apply)) List(TypeTree(Ast.Raw))),
    //   List(Apply(Select(Select(Ident(Ast), TermName(Raw)), TermName(apply)), List(Literal(Constant(1)))))), TermName(isEmpty))
    

    println("START")

    println(body)
    println(show(body))
    //println(s"is pure tree: ${isPureTree(body)}")
    val ast = convert(body)
    /*
    println("===")
    println(ast)
    println("===")
    ast match {
      case Ast.Raw(obj) =>
        println(show(obj.asInstanceOf[Tree]))
       case _ =>
    }
    */
    val astTree = convert2(ast)
    c.Expr[Ast](astTree)
  }
}
