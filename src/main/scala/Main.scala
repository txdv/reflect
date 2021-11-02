import scala.reflect.runtime.universe
import universe._

case class Test(a: Int, b: Boolean, s: String)

object Main extends App {

  val a = 123

  val result = MyFilter.filter[Test] { t =>
    s"a $t" == 123
  }

  /*
  println(result)
  println(result)
  println(result)
  */

  val t = Test(1, true, "2")
  println {
    reify {
      t.a == 0 && t.b == "asd"
    }
  }
}
