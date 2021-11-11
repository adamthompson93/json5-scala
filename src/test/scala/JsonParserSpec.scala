import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import parser.JsonParser._
import parser._

class JsonParserSpec extends AnyFunSpec with Matchers {
  describe("JsonParser") {
    it("will read in a null") {
      val expected = JsonNull
      jsonNull.run("null") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    //jsonBool
    it("will read in a true") {
      val expected = JsonBool(true)
      jsonBool.run("true") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }
    it("will read in a false") {
      val expected = JsonBool(false)
      jsonBool.run("false") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    //JsonNumber
    it("will read in an int") {
      val expected = JsonInt(15)
      jsonNumber.run("15") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }
    it("will read nothing and not error spectacularly") {
      jsonNumber.run("") match {
        case Some(_) => fail
        case None    => succeed
      }
    }
    it("will read in a leading decimal") {
      val expected = JsonDouble(0.56789)
      jsonNumber.run(".56789") shouldBe expected
    }

    it("will read in a trailing decimal") {
      pending
      val expected = JsonDouble(56789)
      jsonNumber.run("56789.") shouldBe expected
    }

//    it("will read in a string") {
//      val expected = JsonString("true")
//      jsonString.run("\"true\"") shouldBe expected
//    }
//
//    it("will do the whole shebang of side reqs") {
//      val expected = JsonObject(
//        mutable.LinkedHashMap(
//          "alphanumeric_field_needs_quotes" -> JsonBool(false),
//          "_leading_underscores_ok" -> JsonBool(true),
//          "trailing_underscores_ok_" -> JsonBool(true),
//          "carriage_return_acts_like_comma" -> JsonBool(true),
//          "vanilla JSON keys also supported" -> JsonBool(true),
//          "a list with null element in at index 1" -> JsonArray(
//            List(
//              JsonInt(0),
//              JsonNull,
//              JsonInt(2),
//              JsonInt(3),
//              JsonInt(4),
//              JsonInt(5)
//            )
//          ),
//          "a list with null element in at index 0" -> JsonArray(
//            List(
//              JsonNull,
//              JsonInt(1),
//              JsonInt(2),
//              JsonInt(3),
//              JsonInt(4),
//              JsonInt(5)
//            )
//          ),
//          "a list with null final element" -> JsonArray(
//            List(
//              JsonInt(0),
//              JsonInt(1),
//              JsonInt(2),
//              JsonInt(3),
//              JsonInt(4),
//              JsonNull
//            )
//          ),
//          "empty list is interpreted as the empty list, not [null]" -> JsonArray(
//            List()
//          )
//        )
//      )
//      jsonObject.run("""{
//                         |  alphanumeric_field_needs_quotes: false,
//                         |  _leading_undderscores_ok: true,
//                         |  trailing_underscores_ok_: true,
//                         |  carriage_return_acts_like_comma: true
//                         |  "vanilla JSON keys also supported": true
//                         |  "a list with null element in at index 1": [0,,2,3,4,5]
//                         |  "a list with null element in at index 0": [,1,2,3,4,5]
//                         |  "a list with null final element": [0,1,2,3,4,]
//                         |  "empty list is interpreted as the empty list, not [null]": []
//                         |  // You can also add comments
//                         |  /* Multiline
//                         |     comments
//                         |     /* can be nested */
//                         |  */
//                         |}""".stripMargin) shouldBe expected
//
//    }
  }
}
