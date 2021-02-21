package advent2019

import org.scalatest.funsuite.AnyFunSuite

class IncodeSpec extends AnyFunSuite {

    test("Intcode: run") {
        var input = "1,0,0,0,99"

        assertResult(2) {
            Intcode.run(input)(0)
        }

        input = "2,3,0,3,99"

        assertResult(6) {
            Intcode.run(input)(3)
        }

        input = "2,4,4,5,99,0"

        assertResult(9801) {
            Intcode.run(input)(5)
        }

        input = "1,1,1,4,99,5,6,0,99"

        assertResult(30) {
            Intcode.run(input)(0)
        }
    }
}

