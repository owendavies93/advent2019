package advent2019

import org.scalatest.funsuite.AnyFunSuite

class Day4Spec extends AnyFunSuite {

    test("Day 4: countValidPasswords") {
        assertResult(1) {
            Day4.part1(111111, 111111)
        }

        assertResult(0) {
            Day4.part1(223450, 223450)
        }

        assertResult(0) {
            Day4.part1(123789, 123789)
        }
    }

    test("Day 4: checkIsolatedPair") {
        assert(Day4.checkIsolatedPair("112233", 0))
        assert(!Day4.checkIsolatedPair("123444", 0))
        assert(Day4.checkIsolatedPair("111122", 0))
    }
}
