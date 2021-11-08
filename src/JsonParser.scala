object JsonParser {
  def parse(json: String) : JsonObject = {

  }
}

sealed trait JsonValue

case object JsonNull extends JsonValue

case class JsonString(str : String) extends JsonValue

sealed trait JsonNumber extends JsonValue

case class JsonInt(num: Int) extends JsonNumber

case class JsonFloat(num: Float) extends JsonNumber

case class JsonDouble(num: Double) extends JsonNumber

/** thing: [,] is an empty  list, and so would be JsonArray(List())
 *
 * @param arr
 */
case class JsonArray(arr : List[JsonValue]) extends JsonValue

case class JsonObject(map: Map[String, JsonValue]) extends JsonValue

