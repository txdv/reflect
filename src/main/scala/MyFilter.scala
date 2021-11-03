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
  case class Bool(value: Boolean) extends Const
  case class Method(ast: Ast, name: String, args: Seq[Ast]) extends Const
}

object MyFilter {
  def filter[T](p: T => Boolean): Ast = macro filterImpl[T]


  def filterImpl[T](c: Context)(p: c.Expr[T => Boolean]): c.Expr[Ast] = {
    import c.universe._

    val `==` = TermName("$eq$eq")
    val `!=` = TermName("$bang$eq")

    val `&&` = TermName("$amp$amp")
    val `||` = TermName("$bar$bar")

    val `<=` = TermName("$less$equal")
    val `>=` = TermName("$greater$equal")

    val Function(args, body) = p.tree
    val ValDef(mods, name, tp, rhs) = args(0)

    def select(s: Tree, name: String, path: Seq[String] = Seq.empty): Ast = s match {
      case Ident(TermName(name)) =>
        Ast.Field(path.reverse.mkString("."))
      case Select(s, TermName(fieldName)) =>
        select(s, name, path :+ fieldName)
    }

    def convert(t: Tree): Ast = {
      t match {
        case s: Select =>
          select(s, name.toString)

        case Literal(Constant(right)) =>
          right match {
            case i: Integer =>
              Ast.Integer(i.intValue)
            case d: Double =>
              Ast.Double(d.doubleValue)
            case s: String =>
              Ast.Str(s)
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
        case Ident(TermName(o)) =>
          Ast.Raw(Ident(TermName(o)))
      }
    }


    def convert2(ast: Ast): Tree = {
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
        case Ast.Double(double) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Double")), TermName("apply")), List(Literal(Constant(double))))

        case Ast.And(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("And")), TermName("apply")), List(convert2(left), convert2(right)))
        case Ast.Or(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Or")), TermName("apply")), List(convert2(left), convert2(right)))

        case Ast.Method(ast, methodName, args) =>
          val args2: List[Tree] = args.toList.map(a => convert2(a))
          val arguments = Apply(Select(Ident(TermName("List")), TermName("apply")), args2)
          val list = List(convert2(ast), Literal(Constant(methodName)), arguments)
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Method")), TermName("apply")), list)
        case Ast.Str(string) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Str")), TermName("apply")), List(Literal(Constant(string))))
        case Ast.Raw(o) =>
          val tree = o.asInstanceOf[Tree]
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Raw")), TermName("apply")), List(tree))
      }
    }
    

    val ast = convert(body)
    c.Expr {
      convert2(ast)
    }
  }
}



