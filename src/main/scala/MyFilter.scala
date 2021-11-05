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
}

object MyFilter {
  def filter[T](p: T => Boolean): Ast = macro filterImpl[T]

  private def isPure(ast: Ast): Boolean = {
    ast match {
      case _: Ast.Raw => true
      case _ => false
    }
  }

  def filterImpl[T](c: Context)(p: c.Expr[T => Boolean])(implicit tg: c.universe.WeakTypeTag[T]): c.Expr[Ast] = {
    import c.universe._

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
        case EmptyTree =>
          "EmptyTree"
        case Function(args, body) =>
          val t = s"Function(${args.map(show)}, $body)"

          println(t)
          t
        case Typed(target, typeTree) =>
          val t = s"Typed(${show(target)}, $typeTree)"
          println(t)
          t
        case If(cond, left, right) =>
          //println(s"a: $a")
          //println(s"b: $b")
          //println(s"c: $c")
          s"If(${show(cond)}, ${show(left)}, ${show(right)})"
        case Match(a, b) =>
          println(a.getClass)
          println(b.getClass)
          println(b.map(show).mkString(", "))
          s"Match($a, ${b.map(show)})"
        case CaseDef(a, b, c) =>
          s"CaseDef(${show(a)}, ${show(b)}, ${show(c)})"
        case UnApply(a, b) =>
          s"UnApply($a, $b)"
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

    def select(s: Tree, name: String, path: Seq[String] = Seq.empty): Ast = s match {
      case Ident(TermName(termName)) if termName == name =>
        Ast.Field(tg.tpe.typeSymbol.name.toString, path.reverse.mkString("."))
      case Select(s, TermName(fieldName)) =>
        select(s, name, path :+ fieldName)
      case Apply(Select(Select(Ident(TermName("scala")), TermName("Predef")), TermName("augmentString")), List(rest)) =>
        select(rest, name, path)
      case other =>
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

    def lnr: Int = {
      (new Exception).getStackTrace.toSeq(1).getLineNumber
    }

    def convert(t: Tree): Ast = {
      t match {
        case s: Select =>
          println(lnr)
          select(s, name.toString)

        case Literal(Constant(right)) =>
          println(lnr)
          Ast.Raw(Literal(Constant(right)))
        case Apply(Select(Apply(klass, List(left)), TermName("contains")), List(right)) =>
          println(lnr)
          klass.toString match {
            case "scala.Predef.intArrayOps" =>
              Ast.In(convert(right), convert(left))
            case "scala.Predef.refArrayOps[String]" =>
              Ast.In(convert(right), convert(left))
          }

        case Apply(Select(Apply(TypeApply(klass, typeTree), List(left)), TermName("contains")), List(right)) =>
          println(lnr)
          klass.toString match {
            case "scala.Predef.refArrayOps" =>
              Ast.In(convert(right), convert(left))
          }
        case Apply(Select(l, op), List(r)) =>
          println(s"$lnr $op")
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
          println(lnr)
          println(show(l))
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
          println(lnr)
          Ast.Raw(Ident(TermName(o)))
        case Apply(TypeApply(Select(target, TermName("contains")), List(typeTree)), List(rest)) =>
          println(lnr)
          Ast.In(convert(rest), Ast.Raw(target))
        case If(cond, left, right) =>
          println(s"$lnr")
          val condAst = pure(cond)
          val leftAst = pure(left)
          val rightAst = pure(right)

          if (isPure(condAst) && isPure(leftAst) && isPure(rightAst)) {
            Ast.Raw(If(cond, left, right))
          } else {
            //println(isPure(left
            //Ast.Raw(If(cond, left, right))
            ???
          }


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
          println(lnr)
          println(s"missing: ${other.getClass} ${show(other)}")
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
          println("--------------")
          println(target)
          println("--------------")
          val t = convert2(target)
          val l = convert2(list)
          Apply(Select(Select(Ident(TermName("Ast")), TermName("In")), TermName("apply")), List(t, l))

      }
    }
    

    println("START")
    println(body)
    println(show(body))
    val ast = convert(body)
    println("===")
    println(ast)
    println("===")
    ast match {
      case Ast.Raw(obj) =>
        println(show(obj.asInstanceOf[Tree]))
       case _ =>
    }
    val astTree = convert2(ast)
    c.Expr[Ast](astTree)
  }
}
