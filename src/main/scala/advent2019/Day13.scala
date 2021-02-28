package advent2019

import scalaadventutils.Problem

object Day13 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day13")
        println(part1(input))
    }

    def part1(input: String): Int = {
        val out = Intcode.run(input).out
        out.zipWithIndex
           .filter(_._2 % 3 == 2)
           .count(_._1 == 2)
    }

}
