package advent2019

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day17Spec extends AnyFunSuite {

    test("Day 17: sumParams") {
        val lines = Problem.parseInputToList("day17-test")

        assertResult(76) {
            Day17.sumParams(Day17.parseCameraView(lines))
        }
    }
}
