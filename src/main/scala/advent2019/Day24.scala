package advent2019

import scalaadventutils.CellulaAutomata
import scalaadventutils.CAUtils
import scalaadventutils.Problem

import scala.collection.mutable.ArrayBuffer

object Day24 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day24")
        println(part1(lines))
        println(part2(lines, 200))
    }

    def part1(lines: List[String]): Int = {
        val ca = parseInput(lines)

        def findRepeat
            ( cur: CellulaAutomata
            , seen: Set[ArrayBuffer[Boolean]])
            : Int =

            if (seen.contains(cur.grid)) biodiversity(cur)
            else {
                def stepFn(x: Int, y: Int): Boolean = {
                    val activeNeighbourCount =
                        cur.nonDiagNeighbours(x, y, false).count {
                            case (x, y) => cur.get(x, y)
                        }

                    if (cur.get(x, y))
                        activeNeighbourCount == 1
                    else
                        activeNeighbourCount == 1 || activeNeighbourCount == 2
                }

                findRepeat(cur.step(stepFn), seen + cur.grid)
            }

        findRepeat(ca, Set[ArrayBuffer[Boolean]]())
    }

    private def biodiversity(ca: CellulaAutomata) =
        (ca.grid zip Iterator.iterate(1)(_ * 2))
            .filter { case (cell, num) => cell }
            .map(_._2).sum

    def part2(lines: List[String], times: Int): Int = {
        val arr = parseInput(lines).grid.toArray
        val init: Levels = Levels(Map(0 -> arr))
        val result = Iterator.iterate(init)(step).drop(times).next()
        result.levels.values.map(_.count(identity)).sum
    }

    def step(levels: Levels): Levels = {
        val newMin = levels.min - 1
        val newMax = levels.max + 1
        val newLevels = Levels(
            levels.levels ++ Map(newMin -> newLevel, newMax -> newLevel)
        )

        val newMap = newLevels.levels.map { case (level, grid) =>
            level ->
            (0 until 5).flatMap(y =>
                (0 until 5).map(x => {
                    val lp = LevelPoint(level, Point(x, y))

                    if (lp.p == Point(2, 2)) false
                    else {
                        val ns = lp.neighbours.filter(newLevels.get)
                        if (newLevels.get(lp))
                            ns.size == 1
                        else
                            ns.size == 1 || ns.size == 2
                    }
                })
            ).toArray
        }
        Levels(newMap)
    }

    private def newLevel = Array.fill(25)(false)

    case class Levels(levels: Map[Int, Array[Boolean]]) {
        def get(lp: LevelPoint): Boolean =
            levels.getOrElse(lp.level, newLevel)(lp.p.y * 5 + lp.p.x)

        def min = levels.keys.min

        def max = levels.keys.max
    }

    case class LevelPoint(level: Int, p: Point) {
        private val nonDiagNeighbourList = List(
            Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1)
        )

        def neighbours: List[LevelPoint] = {
            nonDiagNeighbourList.flatMap(ndn => {
                val n = p + ndn
                n match {
                    case Point(-1, _) =>
                        List(LevelPoint(level - 1, Point(1, 2)))
                    case Point(_, -1) =>
                        List(LevelPoint(level - 1, Point(2, 1)))
                    case Point(5, _)  =>
                        List(LevelPoint(level - 1, Point(3, 2)))
                    case Point(_, 5)  =>
                        List(LevelPoint(level - 1, Point(2, 3)))
                    case Point(2, 2)  =>
                        ndn match {
                            case Point(-1, 0) =>
                                (0 until 5).map(y =>
                                    LevelPoint(level + 1, Point(4, y))
                                )
                            case Point(0, -1) =>
                                (0 until 5).map(x =>
                                    LevelPoint(level + 1, Point(x, 4))
                                )
                            case Point(1, 0)  =>
                                (0 until 5).map(y =>
                                    LevelPoint(level + 1, Point(0, y))
                                )
                            case Point(0, 1)  =>
                                (0 until 5).map(x =>
                                    LevelPoint(level + 1, Point(x, 0))
                                )
                            case _ => throw new Exception(s"ndn")
                        }
                    case _ => List(LevelPoint(level, n))
                }
            })
        }

        override def toString() = s"L: $level - P: $p"
    }

    case class Point(x: Int, y: Int) {
        def +(o: Point) = Point(x + o.x, y + o.y)

        override def toString() = s"($x,$y)"
    }

    def parseInput(lines: List[String]) =
        CAUtils.from2DCharArray(lines, '#')
}
