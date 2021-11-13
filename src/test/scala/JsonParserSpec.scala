import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import parser.JsonParser._
import parser._

class JsonParserSpec extends AnyFunSpec with Matchers {
  describe("JsonParser") {
    it("will read in a null") {
      val expected = JsonObject(Map("not_hing" -> JsonNull))
      jsonValue.run("{not_hing: null}") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    //jsonBool
    it("will read in a true") {
      val expected = JsonBool(true)
      jsonValue.run("true") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }
    it("will read in a false") {
      val expected = JsonBool(false)
      jsonValue.run("false") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    //JsonNumber
    it("will read in an int") {
      val expected = JsonDouble(15)
      jsonValue.run("15") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }
    it("will read nothing and not error spectacularly") {
      jsonValue.run("") match {
        case Some(_) => fail
        case None    => succeed
      }
    }
    it("will get the numbers") {
      val expected = JsonDouble(12345)
      jsonValue.run("12345test") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }
    it("will read -") {
      val expected = JsonDouble(-12345)
      jsonValue.run("-12345") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }
    it("will read in a leading decimal") {
      val expected = JsonDouble(0.56789)
      jsonValue.run(".56789") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    it("will read in a trailing decimal") {
      val expected = JsonDouble(56789)
      jsonValue.run("56789.") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    it("will read in a string") {
      val expected = JsonString("true")
      jsonValue.run("\"true\"") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    it("will read an array") {
      val expected = JsonArray(
        List(
          JsonDouble(0),
          JsonDouble(1),
          JsonDouble(2),
          JsonDouble(3),
          JsonDouble(4),
          JsonDouble(5)
        )
      )
      jsonValue.run("""[0,1,2,3,4,5]""") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }
    it("will give an empty list") {
      val expected = JsonArray(List.empty[JsonValue])
      jsonValue.run("[]") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    it("will read an array with a null element") {
      val expected = JsonArray(
        List(
          JsonNull,
          JsonDouble(0),
          JsonNull,
          JsonDouble(2),
          JsonDouble(3),
          JsonDouble(4),
          JsonDouble(5),
          JsonNull
        )
      )
      jsonValue.run("""[ ,0, ,2,3,4,5, ]""") match {
        case Some(value) => value._2 shouldBe expected
        case None        => fail
      }
    }

    it("will treat a carriage return as a comma for objects") {
      val expected = JsonObject(
        Map(
          "carriage_return_acts_like_comma" -> JsonBool(true),
          "vanilla JSON keys also supported" -> JsonBool(true)
        )
      )
      jsonValue.run(
        "{carriage_return_acts_like_comma: true\r \"vanilla JSON keys also supported\": true}"
      ) shouldBe expected
    }
    it("will handle comments") {
      jsonValue.run("//a comment") match {
        case Some(_) => fail("There should be nothing here")
        case None    => succeed
      }
    }
    it("will handle a multiline comment") {
      jsonValue.run(
        "/*a comment \n that is \"dsfds\" on multiple lines */"
      ) match {
        case Some(_) => fail("There should be nothing here")
        case None    => succeed
      }
    }
    it("will handle nested multiline comments") {
      val expected =
        JsonObject(
          Map(
            "no_thing" -> JsonNull,
            "str" -> JsonString("a string")
          )
        )
      jsonValue.run(
        """{
          |   no_thing: null,
          |   /*a comment
          |  /* excuse me */ that "sadrtas" is on multiple lines */
          |  str: "a string"
          |  }
          |""".stripMargin
      ) match {
        case Some(value) => value shouldBe expected
        case None        => succeed
      }
    }

    it("will do the whole shebang of side reqs") {
      val expected = JsonObject(
        Map(
          "alphanumeric_field_needs_quotes" -> JsonBool(false),
          "_leading_underscores_ok" -> JsonBool(true),
          "trailing_underscores_ok_" -> JsonBool(true),
          "carriage_return_acts_like_comma" -> JsonBool(true),
          "vanilla JSON keys also supported" -> JsonBool(true),
          "a list with null element in at index 1" -> JsonArray(
            List(
              JsonDouble(0),
              JsonNull,
              JsonDouble(2),
              JsonDouble(3),
              JsonDouble(4),
              JsonDouble(5)
            )
          ),
          "a list with null element in at index 0" -> JsonArray(
            List(
              JsonNull,
              JsonDouble(1),
              JsonDouble(2),
              JsonDouble(3),
              JsonDouble(4),
              JsonDouble(5)
            )
          ),
          "a list with null final element" -> JsonArray(
            List(
              JsonDouble(0),
              JsonDouble(1),
              JsonDouble(2),
              JsonDouble(3),
              JsonDouble(4),
              JsonNull
            )
          ),
          "empty list is interpreted as the empty list, not [null]" -> JsonArray(
            List()
          )
        )
      )
      jsonValue.run("""{
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
