package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day10Spec extends AnyFunSuite {

    test("Day 10: part1") {
        val lines = Problem.parseInputToList("day10-test1")

        assertResult(33) {
            Day10.part1(lines)
        }

        val lines2 = Problem.parseInputToList("day10-test2")

        assertResult(210) {
            Day10.part1(lines2)
        }
    }

    test("Day 10: part2") {
        val lines1 = Problem.parseInputToList("day10-test2")

        Day10.part2(lines1, 200)
    }
}
