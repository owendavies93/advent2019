package advent2019

import org.scalatest.funsuite.AnyFunSuite

class Day8Spec extends AnyFunSuite {

    test("Day 8: part1") {
        val input = "123456789012"

        assertResult(1) {
            Day8.part1(input, 2, 3)
        }
    }

    test("Day 8: part2") {
        val input = "0222112222120000"

        val out = List(".#", "#.").mkString("\n")

        assert(Day8.part2(input, 2, 2) == out)
    }
}
