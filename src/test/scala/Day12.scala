package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day12Spec extends AnyFunSuite {

    test("Day 12: applyGravity") {
        val ganymede = Day12.Moon(Day12.Point(3, 0, 0), Day12.Point(0, 0, 0))
        val callisto = Day12.Moon(Day12.Point(5, 0, 0), Day12.Point(0, 0, 0))

        assertResult(Day12.Point(1, 0, 0)) {
            ganymede.applyGravity(Set(callisto)).vel
        }

        assertResult(Day12.Point(-1, 0, 0)) {
            callisto.applyGravity(Set(ganymede)).vel
        }
    }

    test("Day 12: part1") {
        val lines = Problem.parseInputToList("day12-test")

        assertResult(179) {
            Day12.part1(lines, 10)
        }
    }

    test("Day 12: part2") {
        val lines = Problem.parseInputToList("day12-test")

        assertResult(2772) {
            Day12.part2(lines)
        }

        val lines2 = Problem.parseInputToList("day12-test2")

        assertResult(4686774924L) {
            Day12.part2(lines2)
        }
    }
}
