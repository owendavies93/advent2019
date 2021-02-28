package advent2019

import scalaadventutils.Problem

import annotation.tailrec

object Day13 {

    type Point = (Int, Int)
    type Game = Map[Point, Panel]

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day13")
        println(part1(input))
        println(run(input))
    }

    def part1(input: String): Int = {
        val out = Intcode.run(input).out
        out.zipWithIndex
           .filter(_._2 % 3 == 2)
           .count(_._1 == 2)
    }

    def run(input: String): Int = {

        @tailrec
        def run_(g: Game, st: Intcode.State, pos: Point, s: Int): Int =
            // Done
            if (st.halted) s
            // Need joystick move
            else if (st.in.isEmpty) {
                val in = nextInput(pos._1, pos._2)
                val next = Intcode.restart(st, List(in))
                run_(g, next, pos, s)
            } else {
                // Output event
                st.out match {
                    // Score updated
                    case x :: y :: score :: tail if x == -1 && y == 0 =>
                        val next = Intcode.restart(st)
                        run_(g, next, pos, score.toInt)
                    // Draw event
                    case x :: y :: tile :: tail =>
                        val t    = getTile(tile.toInt)
                        val padd = if (t == Paddle) x.toInt else pos._1
                        val ball = if (t == Ball)   x.toInt else pos._2
                        val next = Intcode.restart(st)

                        val g_ = g.updated((x.toInt, y.toInt), t)
                        printGame(g_)
                        println()

                        run_(g, next, (padd, ball), s)
                    case _  =>
                        val in = List(st.out.head)
                        val next = Intcode.restart(st, in)
                        run_(g, next, pos, s)
                }
            }

        val init  = Map[Point, Panel]().withDefaultValue(Empty)
        val start = Intcode.run(input, List(), Map(0L -> 2L), true)
        val iPos  = (0, 0)
        run_(init, start, iPos, 0)
    }

    private def getTile(id: Int): Panel = id match {
        case 0 => Empty
        case 1 => Wall
        case 2 => Block
        case 3 => Paddle
        case 4 => Ball
    }

    // simulate automatic gameplay by guessing a joystick move
    private def nextInput(paddle: Int, ball: Int) = paddle match {
        case x if x > ball  => -1
        case x if x < ball  => 1
        case x if x == ball => 0
    }

    private def printGame(g: Game): Unit = {
        val keys = g.keySet
        val maxx = keys.map(_._1).max
        val maxy = keys.map(_._2).max

        (0 to maxy).foreach(y => {
            (0 to maxx).foreach(x => print(g((x, y)).ch))
            println()
        })
    }

    sealed trait Panel {
        def ch: Char
    }

    case object Empty extends Panel {
        def ch = ' '
    }

    case object Wall extends Panel {
        def ch = '#'
    }

    case object Block extends Panel {
        def ch = '+'
    }

    case object Paddle extends Panel {
        def ch = '-'
    }

    case object Ball extends Panel {
        def ch = 'o'
    }
}
