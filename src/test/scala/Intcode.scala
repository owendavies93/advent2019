package advent2019

import org.scalatest.funsuite.AnyFunSuite

class IntcodeSpec extends AnyFunSuite {

    test("Intcode: run") {
        var input = "1,0,0,0,99"

        assertResult(2) {
            Intcode.run(input)._1(0)
        }

        input = "2,3,0,3,99"

        assertResult(6) {
            Intcode.run(input)._1(3)
        }

        input = "2,4,4,5,99,0"

        assertResult(9801) {
            Intcode.run(input)._1(5)
        }

        input = "1,1,1,4,99,5,6,0,99"

        assertResult(30) {
            Intcode.run(input)._1(0)
        }
    }

    test("Intcode: parseOpCode") {
        var (op, modes) = Intcode.parseOpCode(1002)

        assert(op == 2)

        assertResult((0, 1, 0)) {
            modes
        }

        op    = Intcode.parseOpCode(1)._1
        modes = Intcode.parseOpCode(1)._2

        assert(op == 1)

        assertResult((0, 0, 0)) {
            modes
        }
    }

    test("Intcode: input/output") {
        var input = "3,9,8,9,10,9,4,9,99,-1,8"

        assertResult(1) {
            Intcode.run(input, List(8))._2(0)
        }

        assertResult(0) {
            Intcode.run(input, List(1))._2(0)
        }

        input = "3,3,1108,-1,8,3,4,3,99"
        assertResult(1) {
            Intcode.run(input, List(8))._2(0)
        }

        assertResult(0) {
            Intcode.run(input, List(1))._2(0)
        }
    }

    test("Intcode: jump") {
        var input = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
    }
}

