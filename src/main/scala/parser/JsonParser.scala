package parser

import cats.data.StateT
import cats.data.StateT.pure
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

  def isHexDigit(c: Char): Boolean =
    'a' <= c && c <= 'f' || 'A' <= c && c <= 'F' || c.isDigit

  /**   Sequence is some weirdness that basically inverts the types. So we provide a List[Chars] => List[Parsers]
    *   sequence List[Parser[Char]] => Parser[List[Char]]
    */
  def string(str: String): Parser[String] =
    str.map(char).toList.sequence.map(_.mkString)

  lazy val digit: Parser[Char] = sat(_.isDigit)
  lazy val space: Parser[Unit] = span(sat(_.isWhitespace)).map(_ => ())
  lazy val hex: Parser[Char] = sat(isHexDigit)

  //JsonNull
  lazy val jsonNull: Parser[JsonValue] = string("null").map(_ => JsonNull)

  //JsonBool
  lazy val jsonTrue: Parser[JsonValue] = string("true").map(_ => JsonBool(true))
  lazy val jsonFalse: Parser[JsonValue] =
    string("false").map(_ => JsonBool(false))
  lazy val jsonBool: Parser[JsonValue] = jsonTrue <+> jsonFalse

  //JsonNumber
  //TODO: negative numbers
  lazy val jsonInt: Parser[JsonValue] =
    span1(digit).map(x => JsonInt(x.mkString.toInt))

  //Not sure if I should keep the string or convert to int
  lazy val jsonHexInt: Parser[JsonValue] = string("0x") *> hex
    .replicateA(2)
    .map(_.mkString)
    .map(s => JsonInt(Integer.parseInt(s, 16)))
  //Keeps the string
  lazy val jsonHexString: Parser[JsonValue] = string("0x") *> hex
    .replicateA(2)
    .map(s => JsonHex(s.mkString))

  lazy val minus: Parser[Int] = char('-').map(_ => -1)
  lazy val plus: Parser[Int] = char('+').map(_ => 1)
  lazy val e: Parser[Char] = char('e') <+> char('E')
  lazy val digits = span1(digit).map(_.mkString.toInt)

  //Figure out how to get all of this stuff and then hand it to a function that will construct a double.
  lazy val doubleLiteral: Parser[Double] = (
    minus <+> pure(1),
    digits,
    char('.') *> span1(digit).map(d =>
      (List('0', '.') :++ d).mkString.toDouble
    ) <+> pure(0),
    (e *> (plus <+> minus <+> pure(1)) product digits).map(d =>
      d._1 * d._2
    ) <+> pure(0)
  ).mapN(doubleFromParts)

  def doubleFromParts(
      sign: Int,
      int: Int,
      decimal: Double,
      exponent: Int
  ): Double = sign * (int + decimal) * Math.pow(10, exponent)

  lazy val jsonDouble: Parser[JsonValue] = doubleLiteral.map(d => JsonDouble(d))

  lazy val jsonNumber: Parser[JsonValue] = jsonDouble <+> jsonInt <+> jsonHexInt

  //JsonString
  lazy val quotation: Parser[Char] = string("\\\"").map(_ => '"')
  lazy val solidius: Parser[Char] = string("\\\\").map(_ => '\\')
  lazy val reverseSolidius: Parser[Char] = string("\\/").map(_ => '/')
  lazy val backspace: Parser[Char] = string("\\b").map(_ => '\b')
  lazy val formfeed: Parser[Char] = string("\\f").map(_ => '\f')
  lazy val linefeed: Parser[Char] = string("\\n").map(_ => '\n')
  lazy val carriageReturn: Parser[Char] = string("\\r").map(_ => '\r')
  lazy val horizontalTab: Parser[Char] = string("\\t").map(_ => '\t')
  lazy val escapedUnicode: Parser[Char] =
    hex.replicateA(4).map(s => Integer.parseInt(s.mkString, 16).toChar)
  lazy val unicode: Parser[Char] = string("\\u") *> escapedUnicode
  lazy val escapeChar: Parser[Char] =
    quotation <+>
      solidius <+>
      reverseSolidius <+>
      backspace <+>
      formfeed <+>
      linefeed <+>
      carriageReturn <+>
      horizontalTab <+>
      unicode

  lazy val wildcardChar: Parser[Char] = sat(c => (c != '"') && (c != '\\'))
  lazy val stringLiteral: Parser[String] =
    quotation *> span(wildcardChar <+> escapeChar).map(_.mkString) <* quotation
  lazy val jsonString: Parser[JsonString] = stringLiteral.map(JsonString)

  //JsonArray
  lazy val emptyList: Parser[JsonValue] =
    string("[,]").map(_ => JsonArray(List.empty[JsonValue]))
  def sepBy[A, B](separator: Parser[A], element: Parser[B]): Parser[List[B]] =
    (element, span(separator *> element) <+> pure(List.empty[B]))
      .mapN((x, y) => x +: y)
  lazy val commaOrReturn: Parser[Char] =
    space *> carriageReturn <+> char(',') <* space
  lazy val arrayLiteral: Parser[JsonValue] =
    (char('[') *> space *> sepBy(commaOrReturn, jsonValue) <* space <* char(
      ']'
    ))
      .map(l => JsonArray(l))
  lazy val jsonArray: Parser[JsonValue] = emptyList <+> arrayLiteral

  //JsonObject
  lazy val keyNoQuotes: Parser[String] =
    span(sat(c => c.isLetterOrDigit || c == '_')).map(_.mkString)
  lazy val key: Parser[String] = keyNoQuotes <+> stringLiteral
  lazy val pair: Parser[(String, JsonValue)] =
    (key <* space <* char(':') <* space, jsonValue).mapN((key, value) =>
      key -> value
    )
  lazy val jsonObject: Parser[JsonValue] =
    (char('{') *> space *> sepBy(commaOrReturn, pair) <* space <* char('}'))
      .map { pairs => JsonObject(pairs.toMap) }

  //JsonValue
  val jsonValue: Parser[JsonValue] =
    jsonNull <+> jsonBool <+> jsonNumber <+> jsonObject

  //JsonObject

//  lazy val jsonObject: Parser[JsonValue] = ???

}
