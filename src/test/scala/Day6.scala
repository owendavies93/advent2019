package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day6Spec extends AnyFunSuite {

    test("Day 6: countOrbits") {
        val lines = Problem.parseInputToList("day6-test")

        assertResult(42) {
            Day6.countOrbits(lines)
        }
    }

    test("Day 6: findPath") {
        val lines = Problem.parseInputToList("day6-test2")

        assertResult(4) {
            Day6.findPath(lines)
        }
    }
}
