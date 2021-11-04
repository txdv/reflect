import scala.reflect.runtime.universe
import universe._

case class Test(a: Int, b: Boolean, s: String)
case class Nested(index: Int, t: Test)
case class Encapsulated(index: Int, n: Nested)

object Main {
  def main(args: Array[String]): Unit = {

    val a = args(1)

    val result = MyFilter.filter[Encapsulated] { e =>
      e.n.t.s.length == args(2)

    }

    println(result)
    // where 

  }

}
