package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day20Spec extends AnyFunSuite {

    val lines1 = Problem.parseInputToList("day20-test1")

    test("Day 20: parsing") {
        val (grid, pointMap, start, end) = Day20.parseInput(lines1)

        assertResult(Day20.Point(9, 2)) {
            start
        }

        assertResult(Day20.Point(13, 16)) {
            end
        }

        assert(pointMap.keys.size == 6)
    }

    test("Day 20: findPath") {
        assertResult(23) {
            Day20.findPath(lines1)
        }
    }

    test("Day 20: findMultiLevelPath") {
        assertResult(26) {
            Day20.findMultiLevelPath(lines1)
        }

        val lines2 = Problem.parseInputToList("day20-test2")

        assertResult(396) {
            Day20.findMultiLevelPath(lines2)
        }
    }
}
