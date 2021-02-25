package advent2019

import scalaadventutils.Problem

object Day9 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day9")
        println(Intcode.run(input, List(1)).out(0))
        println(Intcode.run(input, List(2)).out(0))
    }

}
