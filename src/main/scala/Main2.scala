import scala.reflect.runtime.universe
import universe._

sealed trait Ast

object Ast {
  case class Raw(obj: Any) extends Ast
  case class Match(expr: Ast, cases: Seq[Ast]) extends Ast
}

object Main {

  def main(args: Array[String]): Unit = {
    val ast = reify {
      Ast.Match(Ast.Raw(1), Seq(Ast.Raw(2)))
    }

    println(ast)
    println(showRaw(ast))
  }

}
