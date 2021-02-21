package advent2019

import org.scalatest.funsuite.AnyFunSuite

class Day1Spec extends AnyFunSuite {

    test("Day 1: getFuel") {
        assertResult(2) {
            Day1.getFuel(12)
        }

        assertResult(2) {
            Day1.getFuel(14)
        }

        assertResult(654) {
            Day1.getFuel(1969)
        }

        assertResult(33583) {
            Day1.getFuel(100756)
        }
    }

    test("Day 1: getFuelDown") {
        assertResult(2) {
            Day1.getFuelDown(12, 0)
        }

        assertResult(966) {
            Day1.getFuelDown(1969, 0)
        }

        assertResult(50346) {
            Day1.getFuelDown(100756, 0)
        }
    }
}
