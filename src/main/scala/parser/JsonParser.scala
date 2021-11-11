package parser

import cats.data.StateT
import cats.implicits._

import scala.language.postfixOps

object JsonParser {

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

  def span1[A](p: Parser[A]): Parser[List[A]] = for {
    head <- p
    tail <- span(p)
  } yield head :: tail

  def span[A](p: Parser[A]): Parser[List[A]] =
    span1(p) <+> List.empty[A].pure[Parser]

  def char(c: Char): Parser[Char] = sat(_ == c)

  /**   Sequence is some weirdness that basically inverts the types. So we provide a List[Chars] => List[Parsers]
    *   sequence List[Parser[Char]] => Parser[List[Char]]
    */
  def string(str: String): Parser[String] =
    str.map(char).toList.sequence.map(_.mkString)

  val digit: Parser[Char] = sat(_.isDigit)
  val alphaNum: Parser[Char] = sat(_.isLetterOrDigit)
  val space: Parser[Unit] = span(sat(_.isWhitespace)).map(_ => ())

  //JsonNull
  val jsonNull: Parser[JsonValue] = string("null").map(_ => JsonNull)

  //JsonBool
  val jsonTrue: Parser[JsonValue] = string("true").map(_ => JsonBool(true))
  val jsonFalse: Parser[JsonValue] = string("false").map(_ => JsonBool(false))
  val jsonBool: Parser[JsonValue] = jsonTrue <+> jsonFalse

//  //JsonString
//  //TODO: Implement some sort of wildcard?
//  val jsonString: Parser[JsonValue] = ???

  //JsonNumber
  val jsonInt: Parser[JsonValue] =
    span1(digit).map(x => JsonInt(x.mkString.toInt))
//  val containsDot: Parser[JsonValue] = ???
//  val jsonDouble: Parser[JsonValue] = ???
  val jsonNumber: Parser[JsonValue] = jsonInt

//  //JsonValue
//  val jsonValue: Parser[JsonValue] =
//    jsonNull <+> jsonBool <+> jsonNumber

  //JsonObject
  val keyNoQuotes: Parser[Char] = alphaNum <+> char('_')
  val keyWithQuotes: Parser[Char] = char('"')
//  val jsonObject: Parser[JsonValue] = ???

  //JsonArray
  val emptyList: Parser[JsonValue] = string("[,]").map(_ => JsonArray(List()))
  val jsonArray: Parser[JsonValue] = emptyList
}
