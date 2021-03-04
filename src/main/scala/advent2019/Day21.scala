package advent2019

import scalaadventutils.Problem

object Day21 {

    def main(args: Array[String]): Unit = {
        val intcode = Problem.parseInputToString("day21")
        run(intcode, "day21-part1")
        run(intcode, "day21-part2")
    }

    def run(intcode: String, res: String) = {
        val program = readProgram(res)
        val output  = Intcode.run(intcode, program).out

        if (output.last < 128) print(output.map(_.toChar).mkString)
        else println(output.last)
    }

    def readProgram(res: String) =
        Problem.parseInputToList(res)
               .filterNot(_.startsWith("#"))
               .mkString("\n")
               .map(_.toLong).toList

}
