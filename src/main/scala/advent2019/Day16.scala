package advent2019

import scalaadventutils.Problem

import scala.math.abs

object Day16 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day16")
        //println(part1(input, 100))
        println(part2(input, 100))
    }

    def part2(input: String, phases: Int): String = {
        val fullInput = (input * 10000).map(_.toInt - 48)
        val offset = input.take(7).toInt

        Iterator.iterate(fullInput.reverse)(transformPart2)
                .drop(phases)
                .next().reverse.drop(offset).take(8).mkString
    }

    def transformPart2(arr: IndexedSeq[Int]) =
        arr.scanLeft(0)((next, i) => next + i)
           .tail.map(x => abs(x % 10))

    val base = List(0, 1, 0, -1)

    def part1(input: String, phases: Int): String = {
        val start = input.map(_.toInt - 48)
        Iterator.iterate(start)(transform)
                .drop(phases)
                .next().take(8).mkString
    }

    def transform(arr: IndexedSeq[Int]) = {
        arr.zipWithIndex.map { case(i, index) =>
            val pattern = LazyList.continually(
                base.flatMap(x => List.fill(index + 1)(x))
            ).flatten.drop(1)

            abs((arr zip pattern).map(z => z._1 * z._2).sum % 10)
        }
    }
}
