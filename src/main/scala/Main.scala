//import scala.reflect.runtime.universe
//import universe._

case class Test(a: Int, b: Boolean, s: String)
case class Nested(index: Int, t: Test)
case class Encapsulated(index: Int, n: Nested, s: String, b: Boolean)

object Main {

  def main(args: Array[String]): Unit = {

    //val a = args(1)


      //(e.n.t.s.length + (1 + 1 + (args.length + func(e.index)))) == func(args(2).toInt)
    //val e = Encapsulated(1, null, null)
    //val b = Seq(1, 2, 3)
    //val b = args.map(_.toInt).toSeq

      //if (args.nonEmpty) args.map(_.toInt).toSeq.contains(e.index) else true
    val t = Test(3, true, "")
    //val g = Encapsulated(0, Nested(2, Test(3, true, "")), "")

    def func(a: Int)(b: Int)(implicit t: Int): Int = a + 3 + b
    //println(Try)
    val a = args.flatMap(arg => scala.util.Try(arg.toInt).toOption)
    println(a.toSeq)

      //if (a.nonEmpty) a.contains(e.index) else false
      //args.length == 3 && e.b && args.contains("asd")

    val result = MyFilter.filter[Encapsulated] { e =>
      //if (e.index > 10) e.index % 2 == 0 else e.index % 2 == 1
      e match {
        case Encapsulated(a, _, _, _) => a == 0
      }
    }

    println(result)
    /*

    import scala.reflect.runtime.universe._


    println {
      showRaw { 
        reify { Seq[Ast](Ast.Raw(true)) }.tree
      }
    }
    */

    //println(Ast.optimize(result))

    /*
    val resultReal = {
      import Ast._
      Equal(Method(Field("n.t.s"), "length", List.empty), Raw(args(2)))
    }
    */

    // where 
    //
    //
    import scala.reflect.runtime.universe._

    println(showRaw(reify {
      1 match {
        case _ => true
      }
    }))

  }

}
