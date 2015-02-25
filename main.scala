import scala.util.parsing.combinator._

// test
Testing.testAll()

object Testing {
  val tests = List (
    "1, 0xcafe, 0b1010, -10",
    "0xcafebabe + 0xdeadbeef",
    "2 - (10)",
    "1 + -2",
    "1 + 2 + 3 + 4",
    "1 - 1",
    "1 - (2 * 3) + 1",
    "0"
  )
  def testAll(): Unit = tests.foreach(test)

  def test(str: String): Unit =
  YACalc(str) match {
    case Some(result) =>
      import scala.io.AnsiColor._
      val res = result.mkString(", ")
      println(s"$GREEN$str$RESET -> $res")
    case None =>
      import scala.io.AnsiColor._
      println(s"$RED$str$RESET: failed")
  }
}

object YACalc extends RegexParsers {
  type T = Parser[BigInt]
  def integerLiteral: T = (
    binaryNumeral
  | hexNumeral
  | decimalNumeral
  | "0" ^^^ BigInt(0)
  )

  def binaryNumeral: T  = "0b" ~> "[01]+".r ^^ {
    BigInt(_, 2)
  }
  def hexNumeral: T     = "0x" ~> "[0-9a-fA-F]+".r ^^ {
    BigInt(_, 16)
  }
  def decimalNumeral: T = "[1-9][0-9]*".r ^^ {
    BigInt(_)
  }
  def simpleExpr: T = (
    "-" ~> simpleExpr ^^ { f => -f }
  | integerLiteral
  | "(" ~> expr <~ ")"
  )
  def expr      : T = (
    simpleExpr ~ "+" ~ expr ^^ {
      t => t._1._1 + t._2
    }
  | simpleExpr ~ "-" ~ expr ^^ {
      t => t._1._1 - t._2
    }
  | simpleExpr ~ "*" ~ expr ^^ {
      t => t._1._1 * t._2
    }
  | simpleExpr ~ "/" ~ expr ^^ {
      t => t._1._1 / t._2
    }
  | simpleExpr
  )
  def list = repsep(expr, ",")
  def apply(str: String) = Option(parseAll(list, str) getOrElse null)
}

