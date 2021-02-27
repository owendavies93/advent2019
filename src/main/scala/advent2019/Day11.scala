package advent2019

import scalaadventutils.Problem

import annotation.tailrec

object Day11 {

    type ColMap = Map[Point, Long]

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day11")
        println(part1(input))
        part2(input)
    }

    def part1(input: String): Int = {
        val cols  = Map[Point, Long]().withDefaultValue(0L)
        val state = Intcode.run(input, List(0L))
        val start = new Point(0, 0, Direction.U)

        val colour = state.out(0)
        val dir    = state.out(1)

        val res = run(cols.updated(start, colour), state, start.turn(dir).move)

        res.keySet.size
    }

    def part2(input: String): Unit = {
        val cols  = Map[Point, Long]().withDefaultValue(0L)
        val state = Intcode.run(input, List(1L))
        val start = new Point(0, 0, Direction.U)

        val colour = state.out(0)
        val dir    = state.out(1)

        val res = run(cols.updated(start, colour), state, start.turn(dir).move)
        println(gridString(res))
    }

    @tailrec
    def run
        ( cols: ColMap
        , st: Intcode.State
        , loc: Point)
        : ColMap = {

        val col = cols(loc)
        val st_ = Intcode.restart(st, List(col))
        if (st_.halted) cols.updated(loc, st_.out(0))
        else {
            val col_ = st_.out(0)
            val dir_ = st_.out(1)
            run(cols.updated(loc, col_), st_, loc.turn(dir_).move)
        }
    }

    def gridString(cols: ColMap): String = {
        val points = cols.keySet
        val minx = points.map(_.x).min
        val maxx = points.map(_.x).max
        val miny = points.map(_.y).min
        val maxy = points.map(_.y).max

        (maxy to miny by -1).map(y =>
            (minx to maxx).map(x =>
                if (cols(new Point(x, y, Direction.U)) == 1) "#" else "."
            ).mkString
        ).mkString("\n")
    }

    object Direction extends Enumeration {
        val U, D, R, L = Value
    }

    class Point(val x: Int, val y: Int, dir: Direction.Value) extends Equals {
        import Direction._

        def move = dir match {
            case U => new Point(x, y + 1, dir)
            case D => new Point(x, y - 1, dir)
            case L => new Point(x - 1, y, dir)
            case R => new Point(x + 1, y, dir)
        }

        def turn(right: Long) = dir match {
            case U => if (right == 1) new Point(x, y, R) else new Point(x, y, L)
            case D => if (right == 1) new Point(x, y, L) else new Point(x, y, R)
            case L => if (right == 1) new Point(x, y, U) else new Point(x, y, D)
            case R => if (right == 1) new Point(x, y, D) else new Point(x, y, U)
        }

        override def hashCode = x.## * y.##

        override def canEqual(that: Any) = that.isInstanceOf[Point]

        override def equals(that: Any) = that match {
            case p: Point => x == p.x && y == p.y
            case _        => false
        }

        override def toString = "(" + x + "," + y + ")"
    }
}
