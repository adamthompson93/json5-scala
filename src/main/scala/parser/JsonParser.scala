package parser

import cats.data.StateT
import cats.data.StateT.pure
import cats.implicits._

import scala.language.postfixOps

object JsonParser {

  type Parser[A] = StateT[Option, String, A]

  //Parser that just checks if there is a character at all
  val item: Parser[Char] = for {
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
  def notChar(c: Char): Parser[Char] = sat(_ != c)

  def isHexDigit(c: Char): Boolean =
    'a' <= c && c <= 'f' || 'A' <= c && c <= 'F' || c.isDigit

  /**   Sequence is some weirdness that basically inverts the types. So we provide a List[Chars] => List[Parsers]
    *   sequence List[Parser[Char]] => Parser[List[Char]]
    */
  def string(str: String): Parser[String] =
    str.map(char).toList.sequence.map(_.mkString)
  def notString(str: String): Parser[String] =
    str.map(notChar).toList.sequence.map(_.mkString)

  val digit: Parser[Char] = sat(_.isDigit)
  val space: Parser[Unit] = span(sat(_.isWhitespace)).map(_ => ())
  val hex: Parser[Char] = sat(isHexDigit)

  //JsonNull
  val jsonNull: Parser[JsonValue] = string("null").map(_ => JsonNull)

  //JsonBool
  val jsonTrue: Parser[JsonValue] = string("true").map(_ => JsonBool(true))
  val jsonFalse: Parser[JsonValue] =
    string("false").map(_ => JsonBool(false))
  val jsonBool: Parser[JsonValue] = jsonTrue <+> jsonFalse

  //JsonNumber
  //Not sure if I should keep the string or convert to int
  val jsonHexInt: Parser[JsonValue] = string("0x") *> hex
    .replicateA(2)
    .map(_.mkString)
    .map(s => JsonDouble(Integer.parseInt(s, 16)))
  //Keeps the string
  val jsonHexString: Parser[JsonValue] = string("0x") *> hex
    .replicateA(2)
    .map(s => JsonHex(s.mkString))

  val minus: Parser[Int] = char('-').map(_ => -1)
  val plus: Parser[Int] = char('+').map(_ => 1)
  val e: Parser[Char] = char('e') <+> char('E')
  val digits: Parser[Int] = span1(digit).map(_.mkString.toInt)

  //TODO: Figure out how to append a 0 when there is no leading 0 before a decimal
  val doubleLiteral: Parser[Double] = (
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

  val jsonDouble: Parser[JsonValue] = doubleLiteral.map(d => JsonDouble(d))

  val jsonNumber: Parser[JsonValue] = jsonDouble <+> jsonHexInt

  //JsonString
  val quotation: Parser[Char] = string("\\\"").map(_ => '"')
  val solidius: Parser[Char] = string("\\\\").map(_ => '\\')
  val reverseSolidius: Parser[Char] = string("\\/").map(_ => '/')
  val backspace: Parser[Char] = string("\\b").map(_ => '\b')
  val formfeed: Parser[Char] = string("\\f").map(_ => '\f')
  val linefeed: Parser[Char] = string("\\n").map(_ => '\n')
  val carriageReturn: Parser[Char] = string("\\r").map(_ => '\r')
  val horizontalTab: Parser[Char] = string("\\t").map(_ => '\t')
  val escapedUnicode: Parser[Char] =
    hex.replicateA(4).map(s => Integer.parseInt(s.mkString, 16).toChar)
  val unicode: Parser[Char] = string("\\u") *> escapedUnicode
  val escapeChar: Parser[Char] =
    quotation <+>
      solidius <+>
      reverseSolidius <+>
      backspace <+>
      formfeed <+>
      linefeed <+>
      carriageReturn <+>
      horizontalTab <+>
      unicode

  val wildcardChar: Parser[Char] = sat(c => (c != '"') && (c != '\\'))
  val stringLiteral: Parser[String] =
    (char('"') *> span(wildcardChar <+> escapeChar) <* char('"'))
      .map(_.mkString)
  val jsonString: Parser[JsonValue] = stringLiteral.map(s => JsonString(s))

  //Comments
  val newLine: Parser[Char] = char('\f') <+> char('\r') <+> char('\n')
  val comment: Parser[Unit] = (string("//") *> span(
    sat(c => c != '\r' || c != '\n' || c != '\f')
  ) *> newLine).map(_ => ())
  val multiLineComment: Parser[Unit] =
    (string("/*") *> span(notString("*/")) <* string("*/")).map(_ => ())

  val comments: Parser[Unit] = comment <+> multiLineComment

  var jsonValue: Parser[JsonValue] =
    jsonNull <+> jsonBool <+> jsonNumber <+> jsonString

  //TODO: The jsonValue function inside of jsonArray and jsonObject don't contain themselves
  //JsonArray
  val emptyList: Parser[JsonValue] =
    string("[]").map(_ => JsonArray(List.empty[JsonValue]))
  def sepBy[A, B](separator: Parser[A], element: Parser[B]): Parser[List[B]] =
    (element, span1(separator *> element) <+> pure(List.empty[B])).mapN(
      (x, y) => x +: y
    )

  val commaOrReturn: Parser[Char] =
    space *> char(',') <+> newLine <* space

  val arrayLiteral: Parser[JsonValue] =
    (char('[') *> space *> sepBy(
      commaOrReturn,
      jsonValue <+> span(char(' ')).map(_ => JsonNull)
    ) <* space <* char(']'))
      .map(l => JsonArray(l))

  val jsonArray: Parser[JsonValue] =
    emptyList <+> arrayLiteral

  jsonValue = jsonValue <+> jsonArray

  //JsonObject
  val keyNoQuotes: Parser[String] =
    span(sat(c => c.isLetterOrDigit || c == '_')).map(_.mkString)

  val key: Parser[String] = stringLiteral <+> keyNoQuotes
  val pair: Parser[(String, JsonValue)] =
    (key <* space <* char(':') <* space, jsonValue).mapN((key, value) =>
      key -> value
    )
  val jsonObject: Parser[JsonValue] =
    (char('{') *> space *> sepBy(commaOrReturn, pair) <* space <* char('}'))
      .map { pairs => JsonObject(pairs.toMap) }

  //JsonValue
  jsonValue = jsonValue <+> jsonObject
}
