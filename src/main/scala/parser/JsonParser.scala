package parser

class JsonParser {

  sealed trait Parser[A] {
    type parse = String => Option[(String, A)]
  }

  def jsonNull: Parser[JsonValue] = ???

  def charP: Char => Parser[Char] = ???

  case class ParserError(line: Int, char: Int, error: String)
}
