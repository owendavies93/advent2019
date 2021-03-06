package advent2019

import scalaadventutils.Problem

import scala.io.StdIn.readLine

object Day25 {

    def main(args: Array[String]): Unit = {
        val program = Problem.parseInputToString("day25")
        var out = Intcode.run(program)

        while (true) {
            out = run(out)
        }
    }

    def run(st: Intcode.State): Intcode.State = {
        println(toAscii(st.out))
        val input = fromAscii(readLine() + "\n")
        Intcode.restart(st, input)
    }

    private def fromAscii(i: String) = i.map(_.toLong).toList

    private def toAscii(o: List[Long]) = o.map(_.toChar).mkString
}
