package advent2019

import scalaadventutils.Grid
import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import scala.collection.mutable.ArrayBuffer

object Day20 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day20")
        println(findPath(lines))
        println(findMultiLevelPath(lines))
    }

    val portal = """(\.?[A-Z]{2}\.?)""".r

    def findMultiLevelPath(lines: List[String]): Int = {
        val (pg, pointMap, start, end) = parseInput(lines)

        val startL = LevelPoint(start, 0)
        val endL   = LevelPoint(end, 0)

        val g = Map[LevelPoint, Map[LevelPoint, Int]]()

        val graph = new WeightedUndirectedGraph[LevelPoint](g) {
            override def neighbours(l: LevelPoint) = l.neighbours(pg, pointMap)
        }

        val result = graph.searchFrom(startL, (l: LevelPoint) => l == endL)
        result.to.get._2
    }

    def findPath(lines: List[String]): Int = {
        val (pg, pointMap, start, end) = parseInput(lines)

        val g = Map[Point, Map[Point, Int]]()
        val graph = new WeightedUndirectedGraph[Point](g) {
            override def neighbours(p: Point) = p.neighbours(pg, pointMap)
        }

        val result = graph.searchFrom(start, (p: Point) => p == end)
        result.to.get._2
    }

    def parseInput(lines: List[String]): (PortalGrid, PointMap, Point, Point) = {
        val (portals, start, end) = findPortals(lines)
        val height = lines.size
        val width  = lines(0).size

        var minx = width
        var maxx = 0
        var miny = height
        var maxy = 0

        val grid = ArrayBuffer.fill(height * width)(false)
        lines.zipWithIndex.foreach {
            case (line, y) => {
                line.zipWithIndex.foreach {
                    case (c, x) => {
                        val index = y * width + x
                        if (c == '.') {
                            grid(index) = true
                            if (x < minx) minx = x
                            if (x > maxx) maxx = x
                            if (y < miny) miny = y
                            if (y > maxy) maxy = y
                        }
                    }
                }
            }
        }

        val pg = new PortalGrid(
            grid, width, height, Point(minx, miny), Point(maxx, maxy))

        val pointMap = portals.values
                              .map(p => (p._1 -> p._2, p._2 -> p._1))
                              .flatten { case (a,b) => List(a, b) }
                              .toMap

        (pg, pointMap, start, end)
    }

    def findPortals(lines: List[String]): (Portals, Point, Point) = {
        val tran = lines.transpose.map(_.mkString)

        val horiz = portalsInDimension(lines, identity)
        val vert  = portalsInDimension(tran, _.transpose)

        val all = (horiz ++ vert).groupBy(_._1).view
                                 .mapValues(_.map(_._2))
                                 .toMap

        val start   = Portal("AA")
        val end     = Portal("ZZ")
        val startP  = all(start)(0)
        val endP    = all(end)(0)
        val portals = all.filterNot(p => p._1 == start || p._1 == end).view
                         .mapValues(l => (l(0), l(1)))
                         .toMap
        (portals, startP, endP)
    }

    private def portalsInDimension(lines: List[String], f: Point => Point) =
        lines.zipWithIndex.flatMap {
            case (l, i) =>
                portal.findAllMatchIn(l).map(m => {
                    val str = m.matched
                    if (str(0) == '.')
                        (Portal(str.tail), f(Point(m.start, i)))
                    else
                        (Portal(str.slice(0, 2)), f(Point(m.end - 1, i)))
                })
        }

    class PortalGrid
        ( grid: ArrayBuffer[Boolean]
        , width: Int
        , height: Int
        , topCorner: Point
        , bottomCorner: Point
        ) extends Grid(grid, width, height) {

        override def checkBounds(x: Int, y: Int): Boolean =
            x >= topCorner.x && x <= bottomCorner.x &&
            y >= topCorner.y && y <= bottomCorner.y

        def isEdge(x: Int, y: Int): Boolean =
            x == topCorner.x || x == bottomCorner.x ||
            y == topCorner.y || y == bottomCorner.y
    }

    case class LevelPoint(p: Point, level: Int) {
        def neighbours(pg: PortalGrid, ps: PointMap) = {
            val neighbours = pg.nonDiagNeighbours(p.x, p.y, false)
                               .filter(n => pg.get(n._1, n._2))
                               .map(n => LevelPoint(Point(n._1, n._2), level))

            if (ps.contains(p))
                if (!pg.isEdge(p.x, p.y))
                    neighbours :+ LevelPoint(ps(p), level + 1)
                else if (level > 0)
                    neighbours :+ LevelPoint(ps(p), level - 1)
                else neighbours
            else neighbours
        }
    }

    case class Point(x: Int, y: Int) {
        def +(o: Point) = Point(x + o.x, y + o.y)

        def transpose = Point(y, x)

        def neighbours(pg: PortalGrid, ps: PointMap) = {
            val neighbours = pg.nonDiagNeighbours(x, y, false)
                               .filter(n => pg.get(n._1, n._2))
                               .map(n => Point(n._1, n._2))

            if (ps.contains(this)) {
                neighbours :+ ps(this)
            } else {
                neighbours
            }
        }
    }

    case class Portal(symbol: String)

    type Portals = Map[Portal, (Point, Point)]
    type PointMap = Map[Point, Point]
}
