package advent2019

import scalaadventutils.Problem

import annotation.tailrec

object Day7 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day7")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: String) =
        List(0, 1, 2, 3, 4).permutations.map(
            _.foldLeft(0L)((next, i) =>
                Intcode.run(input, List(i, next)).out(0)
            )
        ).max

    def part2(input: String) =
        List(5, 6, 7, 8, 9).permutations.map(settings =>
            feedbackLoop(input, settings)
        ).max

    def feedbackLoop(input: String, settings: List[Int]): Long = {
        val a = Intcode.run(input, List(settings(0), 0))
        val b = Intcode.run(input, settings(1) +: a.out)
        val c = Intcode.run(input, settings(2) +: b.out)
        val d = Intcode.run(input, settings(3) +: c.out)
        val e = Intcode.run(input, settings(4) +: d.out)
        val initialStates = List(a, b, c, d, e)

        @tailrec
        def loop(states: List[Intcode.State]): Long = {
            if (states.exists(_.halted)) states.last.out(0)
            else {
                // We need to define the loop from last to first separately,
                // then we can fold through the rest
                val a_ = Intcode.restart(states.head, states.last.out)
                val states_ = states.tail.foldLeft(List(a_))((next, state) => {
                    next :+ Intcode.restart(state, next.last.out)
                })
                loop(states_)
            }
        }

        loop(initialStates)
    }
}
