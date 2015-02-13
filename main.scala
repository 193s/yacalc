import scala.util.parsing.combinator._

test("0xcafebabe + 0xdeadbeef")
test("123 + 0xff")
test("123 + 1 + 2")

def test(str: String): Unit = {
  val result = YACalc(str) getOrElse "failed"
  import scala.io.AnsiColor._
  println(s"$GREEN$str$RESET -> $result")
}

object YACalc extends RegexParsers {
  type T = Parser[BigInt]
  def integerLiteral =
    hexNumeral | decimalNumeral | "0" ^^^ BigInt(0)

  def decimalNumeral = "[1-9][0-9]*".r ^^ {
    BigInt(_)
  }
  def hexNumeral     = "0x" ~> "[1-9a-f]+".r ^^ {
    BigInt(_, 16)
  }
  def simpleExpr: T = integerLiteral | "(" ~> expr <~ ")"
  def expr      : T = simpleExpr ~ ("+" ~> expr).? ^^ {
    t => t._1 + t._2.getOrElse(0)
  }
  def apply(str: String) = parseAll(expr, str)
}
