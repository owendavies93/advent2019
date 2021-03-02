package advent2019

import scalaadventutils.Problem

object Day17 {

    def main(args: Array[String]): Unit = {
        val program = Problem.parseInputToString("day17")
        println(part1(program))
        part2(program)
    }

    def part1(program: String): Int = {
        val prog = Intcode.run(program).out.map(_.toInt)
        val grid = parseOutput(prog)
        sumParams(grid)
    }

    def part2(program: String): Unit = {
        /*
            Derived the functions for part 2 by hand after inspecting the
            grid and tracing the best path
        */
        val main = "C,B,C,C,B,A,B,A,A,B\n"
        val a    = "L,6,R,6,L,12\n"
        val b    = "R,8,L,12,L,12,R,8\n"
        val c    = "L,12,R,8,L,6,R,8,L,6\n"
        val no   = "n\n"

        val allInputs = List(main, a, b, c, no).map(_.map(_.toLong).toList)

        val state = Intcode.run(program, List(), Map(0L -> 2L))
        val res   = allInputs.foldLeft(state)(
            (nextS, input) => Intcode.restart(nextS, input)
        )
        println(res.out.last)
    }

    def sumParams(g: Grid) = g.findIntersects.map(p => p.x * p.y).sum

    def parseCameraView(lines: List[String]): Grid = {
        val height = lines.size
        val width  = lines(0).size
        val grid = lines.map(_.map(_.toInt)).reduce(_ ++ _).toArray
        Grid(height, width, grid)
    }

    def parseOutput(out: List[Int]): Grid = {
        val width = out.zipWithIndex.find(_._1 == 10).get._2 + 1
        val height = out.size / width
        Grid(height, width, out.toArray)
    }

    def toPos(i: Int): Position = i match {
        case 35 => Scaffold
        case 46 => Open
        case _  => Robot
    }

    case class Grid(height: Int, width: Int, grid: Array[Int]) {
        def get(p: Point) = grid(p.y * width + p.x)

        def neighbours(p: Point) =
            neighbourList.map(_ + p).filter(p =>
                p.x >= 0 && p.y >= 0 && p.x < width && p.y < height)

        def findIntersects: List[Point] =
            (0 until height).flatMap(y =>
                (0 until width).map(x =>
                    Point(x, y)
                )
            ).filter(p => toPos(get(p)) == Scaffold)
             .filter(p => neighbours(p).forall(n => toPos(get(n)) == Scaffold))
             .toList

        override def toString =
            (0 until height).map(y =>
                (0 until width).map(x =>
                    toPos(get(Point(x, y))).ch
                ).mkString
            ).mkString("\n")
    }

    val neighbourList = List(
        Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)
    )

    case class Point(x: Int, y: Int) {
        def +(o: Point) = Point(x + o.x, y + o.y)
    }

    sealed trait Position {
        def ch: Char
    }

    case object Scaffold extends Position {
        def ch = '#'
    }

    case object Open extends Position {
        def ch = '.'
    }

    case object Robot extends Position {
        def ch = '^'
    }
}
