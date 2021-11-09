package parser

import scala.collection.mutable

sealed trait JsonValue

/** val: null */
case object JsonNull extends JsonValue

case class JsonString(str: String) extends JsonValue

case class JsonBool(bool: Boolean) extends JsonValue

/** Numbers may be hexadecimal.
  * Numbers may have a leading or trailing decimal point.
  * Numbers may be IEEE 754 positive infinity, negative infinity, and NaN.
  * Numbers may begin with an explicit plus sign.
  */
sealed trait JsonNumber extends JsonValue

case class JsonInt(num: Int) extends JsonNumber

case class JsonFloat(num: Float) extends JsonNumber

case class JsonDouble(num: Double) extends JsonNumber

case class JsonHex(num: String) extends JsonNumber

/** thing: [,] is an empty  list, and so would be JsonArray(List())
  *
  * @param arr
  */
case class JsonArray(arr: List[JsonValue]) extends JsonValue

case class JsonObject(map: mutable.LinkedHashMap[String, JsonValue])
    extends JsonValue
