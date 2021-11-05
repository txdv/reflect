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
  case class Method(ast: Ast, name: String, args: Seq[Ast]) extends Ast

  case class Raw(obj: Any) extends Ast
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

    // this is awesome
    //println(s"prefix: ${c.prefix}")
    //println(s"isCaseClass: ${tg.tpe.typeSymbol.asClass.isCaseClass}")

    val `==` = TermName("$eq$eq")
    val `!=` = TermName("$bang$eq")

    val `&&` = TermName("$amp$amp")
    val `||` = TermName("$bar$bar")

    val `<=` = TermName("$less$equal")
    val `>=` = TermName("$greater$equal")

    val Function(args, body) = p.tree
    val ValDef(mods, name, tp, rhs) = args(0)

    def select(s: Tree, name: String, path: Seq[String] = Seq.empty): Ast = s match {
      case Ident(TermName(termName)) if termName == name =>
        Ast.Field(path.reverse.mkString("."))
      case Select(s, TermName(fieldName)) =>
        select(s, name, path :+ fieldName)
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
          select(s, name.toString)

        case Literal(Constant(right)) =>
          Ast.Raw(Literal(Constant(right)))
        case Apply(Select(l, op), List(r)) =>
          val leftAst = pure(l)
          val rightAst = pure(r)

          op match {
            case `==` => Ast.Equal(leftAst, rightAst)
            case `!=` => Ast.Unequal(leftAst, rightAst)

            case `&&` => Ast.And(leftAst, rightAst)
            case `||` => Ast.Or(leftAst, rightAst)

            case TermName(methodName) =>
              if (isPure(leftAst)) {
                Ast.Raw(Apply(Select(l, op), List(r)))
              } else {
                Ast.Method(leftAst, methodName, Seq(rightAst))
              }
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

        case Ast.And(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("And")), TermName("apply")), List(convert2(left), convert2(right)))
        case Ast.Or(left, right) =>
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Or")), TermName("apply")), List(convert2(left), convert2(right)))

        case Ast.Method(ast, methodName, args) =>
          val args2: List[Tree] = args.toList.map(a => convert2(a))
          val arguments = Apply(Select(Ident(TermName("List")), TermName("apply")), args2)
          val list = List(convert2(ast), Literal(Constant(methodName)), arguments)
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Method")), TermName("apply")), list)
        case Ast.Raw(o) =>
          val tree = o.asInstanceOf[Tree]
          Apply(Select(Select(Ident(TermName("Ast")), TermName("Raw")), TermName("apply")), List(tree))
      }
    }
    

    val ast = convert(body)
    val astTree = convert2(ast)
    c.Expr[Ast](astTree)
  }
}
