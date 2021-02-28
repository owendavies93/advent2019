package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day14Spec extends AnyFunSuite {

    test("Day 14: part1") {
        val lines1 = Problem.parseInputToList("day14-test1")

        assertResult(31) {
            Day14.part1(lines1)
        }

        val lines2 = Problem.parseInputToList("day14-test2")

        assertResult(13312) {
            Day14.part1(lines2)
        }
    }
}
