package parser

import cats.data.StateT
import cats.implicits._

class JsonParser {

  type Parser[A] = StateT[Option, String, A]

  //Parser that just checks if there is a character at all
  lazy val item: Parser[Char] = for {
    input <- StateT.get[Option, String]
    _ <-
      if (input.nonEmpty)
        StateT.modify[Option, String](_.tail)
      else
        ().raiseError[Parser, Nothing]
  } yield input.head

  //Create a generic parser that can take a boolean check as a value
  def sat(p: Char => Boolean): Parser[Char] = for {
    c <- item
    _ <-
      if (p(c)) c.pure[Parser]
      else ().raiseError[Parser, Nothing]
  } yield c

  def char(c: Char): Parser[Char] = sat(_ == c)

  /**   Sequence is some weirdness that basically inverts the types. So we provide a List[Chars] => List[Parsers]
    *   sequence List[Parser[Char]] => Parser[List[Char]]
    */
  def string(str: String): Parser[String] =
    str.map(char).toList.sequence.map(_.mkString)

  val digit: Parser[Char] = sat(_.isDigit)

}
