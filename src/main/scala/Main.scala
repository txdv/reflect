import scala.reflect.runtime.universe
import universe._

case class Test(a: Int, b: Boolean, s: String)

object Main extends App {
  val a = 123

  val result = MyFilter.filter[Test] { t =>
    t.s.substring(0) == "ASD"
  }

  println(result)
}
