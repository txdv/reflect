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

  case class Field(name: String) extends Ast
  sealed trait Const extends Ast
  case class Integer(value: Int) extends Const
  case class Double(value: java.lang.Double) extends Const
  case class Str(value: java.lang.String) extends Const
  case class Method(ast: Ast, name: String, args: Seq[Ast]) extends Const
}

object MyFilter {
  def filter[T](p: T => Boolean): Ast = macro filterImpl[T]


  def filterImpl[T](c: Context)(p: c.Expr[T => Boolean]): c.Expr[Ast] = {
    println(c.mirror)
    import c.universe._

    val `==` = TermName("$eq$eq")
    val `!=` = TermName("$bang$eq")

    val `&&` = TermName("$amp$amp")
    val `||` = TermName("$bar$bar")

    val `<=` = TermName("$less$equal")
    val `>=` = TermName("$greater$equal")

    def test(func: Function, valDef: ValDef): Unit = {
    }

    val Function(args, body) = p.tree
    val ValDef(mods, name, tp, rhs) = args(0)

    println("===")
    println(p.tree)
    println(p.tree.children)
    println("===")

    def convert(t: Tree): Ast = {
      t match {
        case Select(Ident(TermName(name)), fieldName) =>
          Ast.Field(fieldName.toString)
        case Block(a) =>
          println(s"a: $a")
          ???

        case Literal(Constant(right)) =>
          right match {
            case i: Integer =>
              Ast.Integer(i.intValue)
            case d: Double =>
              Ast.Double(d.doubleValue)
            case s: String =>
              Ast.Str(s)
            case other =>
              println(s"other: $other ${other.getClass}")
              ???
          }
        case Apply(Select(l, op), List(r)) =>
          op match {
            case `==` => Ast.Equal(convert(l), convert(r))
            case `!=` => Ast.Unequal(convert(l), convert(r))

            case `&&` => Ast.And(convert(l), convert(r))
            case `||` => Ast.Or(convert(l), convert(r))

            case TermName(methodName) =>
              Ast.Method(convert(l), methodName, Seq(convert(r)))
          }
        case Apply(Select(l, TermName(methodName)), args) =>
          Ast.Method(convert(l), methodName, args.map(convert))
          //println(s"left: $l ${l.getClass}")
          //println(s"op: $op ${op.getClass}")
          //println(s"right: $r ${r.getClass}")

        case u =>
          println(s"unknown: ${u.getClass} $u")
          u match {
            case Apply(l, r) =>
              println(s"l: $l ${l.getClass}")
              println(s"r: $r ${r.getClass}")
              l match {
                case Select(l1, r1) =>
                  println(s"l1: $l1 ${l1.getClass}")
                  println(s"r1: $r1 ${r1.getClass}")

              }
            case Apply(Select(l1, TermName(funcname)), List()) =>
              println(s"bla: ${funcname}")

            case Typed(expr, t: TypeTree) =>
              println(s"expr: ${expr} ${expr.getClass}")
              println(s"typed: ${t.symbol}")

          }
          ???
      }
    }


    def convert2(ast: Ast): Tree = {
      println(ast)
      ast match {
        case Ast.Ident(value) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Ident")), TermName("apply")), List(Literal(Constant(value))))
        case Ast.Equal(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Equal")), TermName("apply")), List(convert2(left), convert2(right)))
        case Ast.Unequal(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Unequal")), TermName("apply")), List(convert2(left), convert2(right)))
        case Ast.Field(name) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Field")), TermName("apply")), List(Literal(Constant(name))))
        case Ast.Integer(integer) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Integer")), TermName("apply")), List(Literal(Constant(new Integer(integer)))))
        case Ast.And(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("And")), TermName("apply")), List(convert2(left), convert2(right)))
        case Ast.Or(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Or")), TermName("apply")), List(convert2(left), convert2(right)))
        case other =>
          println(s"other: $other")
          ???

      }
    }
    

    val a = reify {
      Ast.Equal(Ast.Field("a"), Ast.Integer(1))
    }

    println(a)

    /*
    println("cia:")
    a match {
      case Expr(Apply(Select(Select(Ident(TermName("Ast")), TermName("Ident")), TermName("apply")), List(Literal(Constant("ASD2"))))) =>

        //println(s"l: $l: ${l.getClass}")
        //println(s"a: $a: ${a.getClass}")
        //println(s"b: $b: ${b.getClass}")
    }

    
    c.Expr(Apply(Select(Select(Ident(TermName("Ast")), TermName("Ident")), TermName("apply")), List(Literal(Constant("ASD22")))))
    */
    val r = convert(body)
    c.Expr(convert2(r))
    /*
    c.Expr {
      Apply(Select(Select(Ident(TermName("Ast")), TermName("Ident")), TermName("apply")), List(Literal(Constant("ASD"))))
      Apply(Select(Select(Ident(TermName("Ast")), TermName("Integer")), TermName("apply")), List(Literal(Constant(new Integer(1)))))
    }*/
  }
}



