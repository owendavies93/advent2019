package advent2019

import scalaadventutils.Problem

import annotation.tailrec
import scala.math.abs

object Day12 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day12")
        println(part1(lines, 1000))
        println(part2(lines))
    }

    def part1(lines: List[String], steps: Int): Int = {
        val moons = parseInput(lines)

        @tailrec
        def step(moons: Set[Moon], i: Int): Int =
            if (i == steps) moons.map(_.totalEnergy).sum
            else step(next(moons), i + 1)

        step(moons, 0)
    }

    def part2(lines: List[String]): Long = {
        val moons = parseInput(lines)
        val f = findCycle(moons) _
        val cx = f(m => (m.pos.x, m.vel.x))
        val cy = f(m => (m.pos.y, m.vel.y))
        val cz = f(m => (m.pos.z, m.vel.z))

        lcm(lcm(cx, cy), cz)
    }

    @tailrec
    private def gcd(a: Long, b: Long): Long =
        if (b == 0) abs(a) else gcd(b, a % b)

    private def lcm(a: Long, b: Long) = abs(a * b) / gcd(a, b)

    type Cache = Map[List[(Int, Int)], Int]

    private def findCycle(moons: Set[Moon])(f: Moon => (Int, Int)): Int = {

        def find_(cache: Cache, iteration: Int, ms: Set[Moon]): Int = {
            val f_ = ms.map(f).toList
            if (cache.get(f_).isDefined) iteration - cache(f_)
            else {
                val cache_ = cache.updated(f_, iteration)
                find_(cache_, iteration + 1, next(ms))
            }
        }

        val init = Map[List[(Int, Int)], Int]()
        find_(init, 1, moons)
    }

    private def next(moons: Set[Moon]): Set[Moon] = {
        val afterGravity = moons.map(m =>
            m.applyGravity(moons - m)
        )
        afterGravity.map(_.move)
    }

    val parser = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r

    def parseInput(lines: List[String]) = lines.map(l => l match {
        case parser(x, y, z) =>
            Moon(Point(x.toInt, y.toInt, z.toInt), Point(0, 0, 0))
    }).toSet

    case class Point(x: Int, y: Int, z: Int) {
        def +(o: Point) = Point(x + o.x, y + o.y, z + o.z)
    }

    case class Moon(pos: Point, vel: Point) {
        def move = copy(pos = pos + vel)

        def applyGravity(others: Set[Moon]) = {
            val change = Point(
                others.toList.map(_.pos.x).map(drag(pos.x, _)).sum,
                others.toList.map(_.pos.y).map(drag(pos.y, _)).sum,
                others.toList.map(_.pos.z).map(drag(pos.z, _)).sum
            )
            copy(vel = vel + change)
        }

        def potential = abs(pos.x) + abs(pos.y) + abs(pos.z)

        def kinetic = abs(vel.x) + abs(vel.y) + abs(vel.z)

        def totalEnergy = potential * kinetic

        private def drag(ours: Int, theirs: Int) =
            if (ours > theirs) -1
            else if (theirs > ours) 1
            else 0

        override def toString =
            s"pos=<x=${pos.x}, y=${pos.y}, z=${pos.z}>, vel=<x=${vel.x}, y=${vel.y}, z=${vel.z}>"
    }
}
