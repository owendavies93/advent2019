package advent2019

import scalaadventutils.Problem

import collection.mutable.ListBuffer
import scala.math.abs

object Day3 {

    type Instr = (Direction.Value, Int)

    object Direction extends Enumeration {
        val U, D, L, R = Value
    }

    import Day3.Direction._

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day3")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[String]) = {
        val first  = parseInput(lines(0))
        val second = parseInput(lines(1))

        val firstPs  = getAllPoints(first).toSet
        val secondPs = getAllPoints(second).toSet

        getMinIntersect(firstPs, secondPs).distanceFromOrigin
    }

    def part2(lines: List[String]) = {
        val first  = parseInput(lines(0))
        val second = parseInput(lines(1))

        val firstPs  = getAllPoints(first)
        val secondPs = getAllPoints(second)

        val intersects = firstPs.toSet & secondPs.toSet - Point(0, 0)
        intersects.map(i => firstPs.indexOf(i) + secondPs.indexOf(i)).min
    }

    def getMinIntersect(f: Set[Point], s: Set[Point]): Point =
        (f & s - Point(0, 0)).minBy(_.distanceFromOrigin)

    def getAllPoints(is: List[Instr]): List[Point] = {
        val points = ListBuffer[Point]()
        val end = is.foldLeft(Point(0, 0))((p, instr) => {
            val next = p.getNext(instr)
            points += p
            points ++= pointsBetweenPoints(p, next)
            next
        })
        (points += end).toList
    }

    def parseInput(line: String): List[Instr] =
        line.split(",").map(i => {
            val d = i(0) match {
                case 'U' => U
                case 'D' => D
                case 'L' => L
                case 'R' => R
            }

            (d, i.tail.toInt)
        }).toList

    def transform(i: Instr): Point = i._1 match {
        case U => Point(i._2 , 0)
        case D => Point(-1 * i._2, 0)
        case L => Point(0 , -1 * i._2)
        case R => Point(0 , i._2)
    }

    def pointsBetweenPoints(prev: Point, next: Point): List[Point] = {
        if (prev.x == next.x) {
            val range = if (prev.y < next.y) (prev.y + 1 until next.y)
                        else (prev.y - 1 until next.y by -1)

            range.map( y => Point(prev.x, y) ).toList
        } else {
            val range = if (prev.x < next.x) (prev.x + 1 until next.x)
                        else (prev.x - 1 until next.x by -1)

            range.map( x => Point(x, prev.y) ).toList
        }
    }

    case class Point(x: Int, y: Int) {
        def add(p: Point) = copy(x = x + p.x, y = y + p.y)

        def distanceFromOrigin = abs(x) + abs(y)

        def getNext(i: Instr) = add(transform(i))

        override def toString(): String = "(" + x + ", " + y + ")"
    }
}
