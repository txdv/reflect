
import org.scalatest._
import flatspec._
import matchers._

case class T(a: Int, b: Boolean, d: Double, s: String)
case class N(t: T)
case class E(n: N)

class Test extends AnyFlatSpec with should.Matchers {

  "filter" should "transform simple equality" in {
    MyFilter.filter[T] { _.a == 1 } should be {
      Ast.Equal(Ast.Field("a"), Ast.Raw(1))
    }
  }

  "filter" should "work with double" in {
    MyFilter.filter[T] { _.d == 1.2 } should be {
      Ast.Equal(Ast.Field("d"), Ast.Raw(1.2))
    }
  }

  "filter" should "work with boolean" in {
    MyFilter.filter[T] { _.b } should be {
      Ast.Field("b")
    }
  }

  "filter" should "transform simple uequality" in {
    MyFilter.filter[T] { _.a != 1 } should be {
      Ast.Unequal(Ast.Field("a"), Ast.Raw(1))
    }
  }

  "filter" should "transform and" in {
    MyFilter.filter[T] { t => t.a != 1 && t.a != 2 } should be {
      Ast.And(
        Ast.Unequal(Ast.Field("a"), Ast.Raw(1)),
        Ast.Unequal(Ast.Field("a"), Ast.Raw(2))
      )
    }
  }

  "filter" should "transform or" in {
    MyFilter.filter[T] { t => t.a == 1 || t.a == 2 } should be {
      Ast.Or(
        Ast.Equal(Ast.Field("a"), Ast.Raw(1)),
        Ast.Equal(Ast.Field("a"), Ast.Raw(2))
      )
    }
  }

  "filter" should "transform function" in {
    MyFilter.filter[T] { t => t.s.substring(10, 3) == "ASD" } should be {
      Ast.Equal(
        Ast.Method(Ast.Field("s"), "substring", List(Ast.Raw(10), Ast.Raw(3))),
        Ast.Raw("ASD")
      )
    }
  }

  "filter" should "transform function with no method" in {
    MyFilter.filter[T] { t => t.s.toLowerCase == "asd" } should be {
      Ast.Equal(
        Ast.Method(Ast.Field("s"), "toLowerCase", List()),
        Ast.Raw("asd")
      )
    }
  }

  "filter" should "handle variables from out of scope" in {
    val a = "asd"
    MyFilter.filter[T] { t => t.s == a } should be {
      Ast.Equal(Ast.Field("s"), Ast.Raw("asd"))
    }
  }

  /*"filter" should "handle variables from out of scope 2" in {
    val a = "asd"
    MyFilter.filter[T] { t => t.s + "asd" == a } should be {
      Ast.Equal(Ast.Field("s"), Ast.Raw("asd"))
    }
  }*/

  "filter" should "handle nested structures" in {
    MyFilter.filter[N] { n => n.t.s == "123" } should be {
      Ast.Equal(Ast.Field("t.s"), Ast.Raw("123"))
    }
  }

  "filter" should "handle double nested structures" in {
    MyFilter.filter[E] { e => e.n.t.s == "123" } should be {
      Ast.Equal(Ast.Field("n.t.s"), Ast.Raw("123"))
    }
  }

  "filter" should "handle more complex pure side" in {
    val args = Seq("321", "123")
    MyFilter.filter[E] { e => e.n.t.s == args(1) } should be {
      Ast.Equal(Ast.Field("n.t.s"), Ast.Raw("123"))
    }
  }

}
