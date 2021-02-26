package advent2019

import scalaadventutils.Problem

import scala.math.abs
import scala.math.atan2

object Day10 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day10")
        println(part1(lines))
    }

    def part1(lines: List[String]): Int = {
        val pointSet = parseInput(lines)
        pointSet.map(p => p.seenBy(pointSet - p).size).max
    }

    def part2(lines: List[String], index: Int): Int = {
        val pointSet = parseInput(lines)
        val best = pointSet.map(p => (p, p.seenBy(pointSet - p)))
                           .maxBy(_._2.size)

        val centre = best._1
        val sorted = (best._2 + centre).toList
            .map(p => (atan2(p.x, p.y), p))
            .sortBy({ case (tan, Point(x, y)) => (tan, x, y) })

        val chosen = sorted(index)._2
        chosen.x * 100 + chosen.y
    }

    def parseInput(lines: List[String]): Set[Point] = {
        val height = lines.size
        val width  = lines(0).size

        (0 until height).flatMap(y =>
            (0 until width).map(x =>
                if (lines(y)(x) == '#') Some(Point(x, y)) else None
            )
        ).filter(_.isDefined).map(_.get).toSet
    }

    case class Point(x: Int, y: Int) {
        def seenBy(ps: Set[Point]): Set[Point] = {
            ps.map(p => {
                val diff    = difference(p)
                val divisor = abs(diff.gcd)
                Point(diff.x / divisor, diff.y / divisor)
            })
        }

        def difference(that: Point) = Point(that.x - x, that.y - y)

        def gcd: Int = if (y == 0) x else Point(y, x % y).gcd

        override def toString = "(" + x + "," + y + ")"
    }
}
