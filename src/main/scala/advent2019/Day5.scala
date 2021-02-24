package advent2019

import scalaadventutils.Problem

object Day5 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day5")
        var out = Intcode.run(input, List(1))._2
        println(out)

        out = Intcode.run(input, List(5))._2
        println(out)
    }
}
