package advent2019

import org.scalatest.funsuite.AnyFunSuite

class Day3Spec extends AnyFunSuite {

    val lines = List("R8,U5,L5,D3", "U7,R6,D4,L4")

    test("Day 3: part1") {
        assertResult(6) {
            Day3.part1(lines)
        }
    }

    test("Day 3: part2") {
        assertResult(30) {
            Day3.part2(lines)
        }
    }
}
