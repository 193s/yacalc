import scala.util.parsing.combinator._

test("1, 1, 0xcafe, 0xbe")
test("0xcafebabe + 0xdeadbeef")
test("1 + 2 + 3")
test("1 - 1")

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
  def hexNumeral: T     = "0x" ~> "[1-9a-fA-F]+".r ^^ {
    BigInt(_, 16)
  }
  def decimalNumeral: T = "[1-9][0-9]*".r ^^ {
    BigInt(_)
  }
  def simpleExpr: T = integerLiteral | "(" ~> expr <~ ")"
  def expr      : T = simpleExpr ~ ("+" ~> expr).? ^^ {
    t => t._1 + t._2.getOrElse(0)
  }
  def list = repsep(expr, ",")
  def apply(str: String) = Option(parseAll(list, str) getOrElse null)
}

