val f: String => String = { case "ping" => "pong" }
f("ping")
//f("abc")

val f2: PartialFunction[String, String] = { case "ping" => "pong" }

f2.isDefinedAt("ping")
f2.isDefinedAt("abc")

val f3: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: y :: rest => "two"
}

f3.isDefinedAt(List(1, 2, 3))

val g: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest =>
    rest match {
      case Nil => "two"
    }
}

g.isDefinedAt(List(1, 2, 3))