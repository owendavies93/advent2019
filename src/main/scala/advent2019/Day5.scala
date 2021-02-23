package advent2019

import scalaadventutils.Problem

object Day5 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day5")
        Intcode.run(input, List(1))
        Intcode.run(input, List(5))
    }
}
