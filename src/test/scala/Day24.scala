package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day24Spec extends AnyFunSuite {

    val lines = Problem.parseInputToList("day24-test")

    test("Day 24: part1") {
        assertResult(2129920) {
            Day24.part1(lines)
        }
    }

    test("Day 24: part2") {
        assertResult(99) {
            Day24.part2(lines, 10)
        }
    }
}
