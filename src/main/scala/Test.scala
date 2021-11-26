
import org.scalatest._
import flatspec._
import matchers._

case class T(a: Int, b: Boolean, d: Double, s: String)
case class N(t: T)
case class E(n: N)

class Test extends AnyFlatSpec with should.Matchers {

  "filter" should "transform simple equality" in {
    MyFilter.filter[T] { _.a == 1 } should be {
      Ast.Equal(Ast.Field("T", "x$1", "a"), Ast.Raw(1))
    }
  }

  "filter" should "work with double" in {
    MyFilter.filter[T] { _.d == 1.2 } should be {
      Ast.Equal(Ast.Field("T", "x$2", "d"), Ast.Raw(1.2))
    }
  }

  "filter" should "work with boolean" in {
    MyFilter.filter[T] { _.b } should be {
      Ast.Field("T", "x$3", "b")
    }
  }

  "filter" should "transform simple uequality" in {
    MyFilter.filter[T] { _.a != 1 } should be {
      Ast.Unequal(Ast.Field("T", "x$4", "a"), Ast.Raw(1))
    }
  }

  "filter" should "transform and" in {
    MyFilter.filter[T] { t => t.a != 1 && t.a != 2 } should be {
      Ast.And(
        Ast.Unequal(Ast.Field("T", "t", "a"), Ast.Raw(1)),
        Ast.Unequal(Ast.Field("T", "t", "a"), Ast.Raw(2))
      )
    }
  }

  "filter" should "transform or" in {
    MyFilter.filter[T] { t => t.a == 1 || t.a == 2 } should be {
      Ast.Or(
        Ast.Equal(Ast.Field("T", "t", "a"), Ast.Raw(1)),
        Ast.Equal(Ast.Field("T", "t", "a"), Ast.Raw(2))
      )
    }
  }

  "filter" should "transform function" in {
    MyFilter.filter[T] { t => t.s.substring(10, 3) == "ASD" } should be {
      Ast.Equal(
        Ast.Method(Ast.Field("T", "t", "s"), "substring", List(Ast.Raw(10), Ast.Raw(3))),
        Ast.Raw("ASD")
      )
    }
  }

  "filter" should "transform function with no method" in {
    MyFilter.filter[T] { t => t.s.toLowerCase == "asd" } should be {
      Ast.Equal(
        Ast.Method(Ast.Field("T", "t", "s"), "toLowerCase", List()),
        Ast.Raw("asd")
      )
    }
  }

  "filter" should "handle variables from out of scope" in {
    val a = "asd"
    MyFilter.filter[T] { t => t.s == a } should be {
      Ast.Equal(Ast.Field("T", "t", "s"), Ast.Raw("asd"))
    }
  }

  //"filter" should "handle variables from out of scope 2" in {
  //  val a = "asd"
  //  MyFilter.filter[T] { t => t.s + "asd" == a } should be {
  //    Ast.Equal(Ast.Field("s"), Ast.Raw("asd"))
  //  }
  //}

  "filter" should "handle nested structures" in {
    MyFilter.filter[N] { n => n.t.s == "123" } should be {
      Ast.Equal(Ast.Field("N", "n", "t.s"), Ast.Raw("123"))
    }
  }

  "filter" should "handle double nested structures" in {
    MyFilter.filter[E] { e => e.n.t.s == "123" } should be {
      Ast.Equal(Ast.Field("E", "e", "n.t.s"), Ast.Raw("123"))
    }
  }

  "filter" should "handle more complex pure side" in {
    val args = Seq("321", "123")
    MyFilter.filter[E] { e => e.n.t.s == args(1) } should be {
      Ast.Equal(Ast.Field("E", "e", "n.t.s"), Ast.Raw("123"))
    }
  }

  "filter" should "handle seq contains" in {
    val args = Seq(1, 2, 3)
    MyFilter.filter[T] { t => args.contains(t.a) } should be {
      Ast.In(Ast.Field("T", "t", "a"), Ast.Raw(args))
    }
  }

  "filter" should "handle int seq contains" in {
    val args = Array("1", "2", "3").map(_.toInt)
    MyFilter.filter[T] { t => args.contains(t.a) } should be {
      Ast.In(Ast.Field("T", "t", "a"), Ast.Raw(args))
    }
  }

  "filter" should "handle array contains" in {
    val args = Array("1", "2", "3")
    MyFilter.filter[T] { t => args.contains(t.s) } should be {
      Ast.In(Ast.Field("T", "t", "s"), Ast.Raw(args))
    }
  }

  "filter" should "handle if statement with pure cond when cond is true" in {
    val args = Array(1, 2, 3)
    MyFilter.filter[T] { t =>
      if (args.nonEmpty) args.contains(t.a) else false
    } should be {
      Ast.In(Ast.Field("T", "t", "a"), Ast.Raw(args))
    }
  }

  "filter" should "handle if statement with pure cond when cond is false" in {
    val args = Array()
    MyFilter.filter[T] { t =>
      if (args.nonEmpty) args.contains(t.a) else false
    } should be {
      Ast.Raw(false)
    }
  }

  "filter" should "handle if statement if cond has tainted expression" in {
    val args = Array()
    MyFilter.filter[T] { t =>
      if (args.contains(t.a)) true else false
    } should be {
      Ast.If(Ast.In(Ast.Field("T", "t", "a"), Ast.Raw(Seq.empty)), Ast.Raw(true), Ast.Raw(false))
    }
  }

  "filter" should "handle match statement if cond is true" in {
    val args = Array()
    MyFilter.filter[T] { t =>
      args.size match {
        case 0 => true
        case _ => false
      }
    } should be {
      Ast.Raw(true)
    }
  }

  "filter" should "handle match statement if cond is false" in {
    val args = Array(1)
    MyFilter.filter[T] { t =>
      args.size match {
        case 0 => true
        case _ => false
      }
    } should be {
      Ast.Raw(false)
    }
  }

  "match" should "work with basic wild card case" in {
    MyFilter.filter[T] { t =>
      t match {
        case _ => false
      }
    } should be {
      Ast.Match(Ast.Field("T", "t", ""), Seq(
        Ast.CaseDef(Ast.WildCard, Ast.Raw(null), Ast.Raw(false)),
      ))
    }
  }

  "filter" should "handle match statement if cond is impure" in {
    MyFilter.filter[T] { t =>
      t.a match {
        case 0 => true
        case _ => false
      }
    } should be {
      Ast.Match(Ast.Field("T", "t", "a"), Seq(
        Ast.CaseDef(Ast.Raw(0), Ast.Raw(null), Ast.Raw(true)),
        Ast.CaseDef(Ast.WildCard, Ast.Raw(null), Ast.Raw(false))
      ))
    }
  }

  "filter" should "handle match statement if cond is impure and case def expr is complex" in {
    MyFilter.filter[T] { t =>
      t.a match {
        case 0 => t.b
        case _ => t.d == "hello"
      }
    } should be {
      Ast.Match(Ast.Field("T", "t", "a"), Seq(
        Ast.CaseDef(Ast.Raw(0), Ast.Raw(null), Ast.Field("T", "t", "b")),
        Ast.CaseDef(Ast.WildCard, Ast.Raw(null), Ast.Equal(Ast.Field("T", "t", "d"), Ast.Raw("hello")))
      ))
    }
  }

  "filter" should "handle inline matching" in {
    MyFilter.filter[T] {
      case _ => false
    } should be {
      Ast.Match(Ast.Field("T","x0$1",""), List(Ast.CaseDef(Ast.WildCard,Ast.Raw(null),Ast.Raw(false))))
    }
  }

  "filter" should "handle concrete inline matching" in {
    MyFilter.filter[Int] {
      case 1 => true
      case 9 => true
      case _ => false
    } should be {
      Ast.Match(Ast.Field("Int", "x0$2", ""),List(
        Ast.CaseDef(Ast.Raw(1), Ast.Raw(null), Ast.Raw(true)),
        Ast.CaseDef(Ast.Raw(9), Ast.Raw(null), Ast.Raw(true)),
        Ast.CaseDef(Ast.WildCard, Ast.Raw(null), Ast.Raw(false)))
      )
    }
  }

  "filter" should "handle empty case class mapping" in {
    MyFilter.filter[T] {
      case T(_, _, _, _) => false
    } should be {
      Ast.Match(Ast.Field("T","x0$3",""), List(
        Ast.CaseDef(Ast.Apply("T", List(Ast.WildCard, Ast.WildCard, Ast.WildCard, Ast.WildCard)), Ast.Raw(null), Ast.Raw(false)))
      )
    }
  }

  /*
  "filter" should "handle match statement if cond is impure and case def expr is complex" in {
    MyFilter.filter[T] { t =>
      t match {
        case T(a, b, _, _) if a == 0 => b
        case T(_, _, _, d) => d == "hello"
      }
    } should be {
      Ast.Match(Ast.Field("T", "a"), Seq(
        Ast.CaseDef(Ast.Raw(0), Ast.Raw(null), Ast.Field("T", "b")),
        Ast.CaseDef(Ast.WildCard, Ast.Raw(null), Ast.Equal(Ast.Field("T", "d"), Ast.Raw("hello")))
      ))
    }
  }
  */

}
