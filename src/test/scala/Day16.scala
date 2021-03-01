package advent2019

import org.scalatest.funsuite.AnyFunSuite

class Day16Spec extends AnyFunSuite {

    test("Day16: transform") {
        val input = "12345678".map(_.toInt - 48)
        assertResult("48226158") {
            Day16.transform(input).mkString
        }
    }

    test("Day16: part1") {
        assertResult("01029498") {
            Day16.part1("12345678", 4)
        }

        assertResult("24176176") {
            Day16.part1("80871224585914546619083218645595", 100)
        }
    }
}
