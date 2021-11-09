import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import parser._

import scala.collection.mutable

class JsonParserSpec extends AnyFunSpec with Matchers {
  describe("JsonParser") {
    it("will read in a null") {
      val expected = JsonObject(mutable.LinkedHashMap("val: null" -> JsonNull))
      JsonParser.parse("val: ") shouldBe expected
    }

    it("will read in a boolean") {
      val expected = JsonObject(mutable.LinkedHashMap("bool" -> JsonBool(true)))
      JsonParser.parse("bool: true") shouldBe expected
    }

    it("will read in a string") {
      val expected =
        JsonObject(mutable.LinkedHashMap("str" -> JsonString("true")))
      JsonParser.parse("{str: \"true\"}") shouldBe expected
    }

    it("will read in an int") {
      val expected = JsonObject(mutable.LinkedHashMap("num" -> JsonInt(15)))
      JsonParser.parse("{num: 15}") shouldBe expected
    }

    it("will read in a leading decimal") {
      val expected =
        JsonObject(mutable.LinkedHashMap("num" -> JsonDouble(0.56789)))
      JsonParser.parse("{num: .56789}") shouldBe expected
    }

    it("will read in a trailing decimal") {
      val expected =
        JsonObject(mutable.LinkedHashMap("num" -> JsonDouble(56789)))
      JsonParser.parse("{num: 56789.}") shouldBe expected
    }

    it("will read in a hexidecimal number") {}
    it("will read in ") {}

    it("will do the whole shebang of side reqs") {
      val expected = JsonObject(
        mutable.LinkedHashMap(
          "alphanumeric_field_needs_quotes" -> JsonBool(false),
          "_leading_underscores_ok" -> JsonBool(true),
          "trailing_underscores_ok_" -> JsonBool(true),
          "carriage_return_acts_like_comma" -> JsonBool(true),
          "vanilla JSON keys also supported" -> JsonBool(true),
          "a list with null element in at index 1" -> JsonArray(
            List(
              JsonInt(0),
              JsonNull,
              JsonInt(2),
              JsonInt(3),
              JsonInt(4),
              JsonInt(5)
            )
          ),
          "a list with null element in at index 0" -> JsonArray(
            List(
              JsonNull,
              JsonInt(1),
              JsonInt(2),
              JsonInt(3),
              JsonInt(4),
              JsonInt(5)
            )
          ),
          "a list with null final element" -> JsonArray(
            List(
              JsonInt(0),
              JsonInt(1),
              JsonInt(2),
              JsonInt(3),
              JsonInt(4),
              JsonNull
            )
          ),
          "empty list is interpreted as the empty list, not [null]" -> JsonArray(
            List()
          )
        )
      )
      JsonParser.parse("""{
                         |  alphanumeric_field_needs_quotes: false,
                         |  _leading_undderscores_ok: true,
                         |  trailing_underscores_ok_: true,
                         |  carriage_return_acts_like_comma: true
                         |  "vanilla JSON keys also supported": true
                         |  "a list with null element in at index 1": [0,,2,3,4,5]
                         |  "a list with null element in at index 0": [,1,2,3,4,5]
                         |  "a list with null final element": [0,1,2,3,4,]
                         |  "empty list is interpreted as the empty list, not [null]": []
                         |  // You can also add comments
                         |  /* Multiline 
                         |     comments
                         |     /* can be nested */
                         |  */
                         |}""".stripMargin) shouldBe expected

    }
  }
}
