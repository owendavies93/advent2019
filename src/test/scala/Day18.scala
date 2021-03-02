package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day18Spec extends AnyFunSuite {
    val lines1 = Problem.parseInputToList("day18-test1")

    test("Day 18: parseInput") {
        val state = Day18.parseInput(lines1)

        assert(state.pos == List(Day18.Point(5, 1)))
        assert(state.keyLocs.keySet.size == 2)
        assert(state.doorLocs.keySet.size == 1)
    }

    test("Day 18: findShortestPath") {
        assertResult(8) {
            Day18.findShortestPath(lines1)
        }

        val lines2 = Problem.parseInputToList("day18-test2")

        assertResult(86) {
            Day18.findShortestPath(lines2)
        }
    }

    test("Day 18: multiple robots") {
        val lines3 = Problem.parseInputToList("day18-test3")

        assertResult(24) {
            Day18.findShortestPath(lines3)
        }

        val lines4 = Problem.parseInputToList("day18-test4")

        assertResult(72) {
            Day18.findShortestPath(lines4)
        }
    }
}
