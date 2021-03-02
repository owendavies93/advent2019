package advent2019

import scalaadventutils.Problem

import annotation.tailrec

object Day19 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day19")
        println(part1(input, 50))
        println(part2(input, 99))
    }

    def part1(program: String, limit: Int) =
        (0 until limit).flatMap(y =>
            (0 until limit).map(x =>
                checkPoint(Point(x, y), program)
            )
        ).sum

    def part2(program: String, size: Int): Int = {
        @tailrec
        def findSquare(cur: Point): Point =
            if (inBeam(cur + Point(-size, size), program))
                cur + Point(-size, 0)
            else if (inBeam(cur.right, program))
                findSquare(cur.right)
            else findSquare(cur.down)

        val start = Point(500, 500)
        val point = findSquare(start)
        point.x * 10000 + point.y
    }

    def inBeam(p: Point, program: String) = checkPoint(p, program) == 1

    def checkPoint(p: Point, program: String) =
        Intcode.run(program, List(p.x.toLong, p.y.toLong)).out(0)

    case class Point(x: Int, y: Int) {
        def +(o: Point) = Point(x + o.x, y + o.y)

        def right = this + Point(1, 0)

        def down = this + Point(0, 1)

        override def toString = "(" + x + "," + y + ")"
    }
}

