
import org.scalatest._
import flatspec._
import matchers._

case class T(a: Int, b: Boolean, s: String)

class Test extends AnyFlatSpec with should.Matchers {
  "filter" should "create equal filter" in {
    MyFilter.filter[T] { _.a == 1 } should be {
      Ast.Equal(Ast.Field("a"), Ast.Integer(1))
    }

  }
}
