
import org.scalatest._
import flatspec._
import matchers._

case class T(a: Int, b: Boolean, s: String)

class Test extends AnyFlatSpec with should.Matchers {
  "filter" should "transform simple equality" in {
    MyFilter.filter[T] { _.a == 1 } should be {
      Ast.Equal(Ast.Field("a"), Ast.Integer(1))
    }
  }

  "filter" should "transform simple uequality" in {
    MyFilter.filter[T] { _.a != 1 } should be {
      Ast.Unequal(Ast.Field("a"), Ast.Integer(1))
    }
  }

  "filter" should "transform and" in {
    MyFilter.filter[T] { t => t.a != 1 && t.a != 2 } should be {
      Ast.And(
        Ast.Unequal(Ast.Field("a"), Ast.Integer(1)),
        Ast.Unequal(Ast.Field("a"), Ast.Integer(2))
      )
    }
  }
}