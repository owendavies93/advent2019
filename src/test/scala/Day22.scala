package advent2019

import scalaadventutils.CircularList
import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day22Spec extends AnyFunSuite {

    val deck = CircularList(10)(0,1,2,3,4,5,6,7,8,9)

    test("Day 22: deal") {
        assertResult(CircularList(deck.size)(9,8,7,6,5,4,3,2,1,0)) {
            Day22.deal(deck)
        }
    }

    test("Day 22: cut") {
        assertResult(CircularList(deck.size)(3,4,5,6,7,8,9,0,1,2)) {
            Day22.cut(deck, 3)
        }

        assertResult(CircularList(deck.size)(6,7,8,9,0,1,2,3,4,5)) {
            Day22.cut(deck, -4)
        }
    }

    test("Day 22: dealIncrement") {
        assertResult(CircularList(deck.size)(0,7,4,1,8,5,2,9,6,3)) {
            Day22.dealIncrement(deck, 3)
        }
    }

    test("Day 22: run") {
        val lines1 = Problem.parseInputToList("day22-test1")

        assertResult(1) {
            Day22.run(lines1, 10, 3)
        }

        val lines2 = Problem.parseInputToList("day22-test2")

        assertResult(1) {
            Day22.run(lines2, 10, 2)
        }
    }
}
