package advent2019

import scalaadventutils.Grid
import scalaadventutils.GridUtils
import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import annotation.tailrec

object Day18 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day18")
        println(findShortestPath(lines))

        val lines2 = Problem.parseInputToList("day18-part2")
        println(findShortestPath(lines2))
    }

    // Same caveats about needless map initialisation apply here as they
    // do in Day 15
    def findShortestPath(lines: List[String]): Int = {
        val startingState = parseInput(lines)
        val stateMap = Map[State, Map[State, Int]]()
        val stateGraph = new WeightedUndirectedGraph[State](stateMap) {
            // The pseudo-Dijkstra algorithm uses get() instead of neighbours()
            // because we need a map of nodes to distances, not just an
            // iterable of nodes
            override def get(s: State) = s.getNeighbouringStates
        }

        val result = stateGraph.searchFromWithDistances(startingState,
            (s: State) => s.keyLocs.isEmpty)

        val endState = result.to.get
        endState._2
    }

    case class State
        ( grid: Grid
        , keyLocs: KeyMap
        , doorLocs: DoorMap
        , pos: List[Point]) {

        def getNeighbouringStates: Map[State, Int] = {
            val pointMap = Map[Point, Map[Point, Int]]()
            val pointGraph =  new WeightedUndirectedGraph[Point](pointMap) {
                override def neighbours(p: Point) =
                    p.neighbours(grid, keyLocs, doorLocs)
            }

            val distances = pos.zipWithIndex.flatMap({
                case (p, index) =>
                    pointGraph.traverseFrom(p).distances.map({
                        case (p_, distance) => p_ -> (index, distance)
                    })
            }).toMap

            keyLocs.filter(kl => distances.contains(kl._1)).map {
                case (p_, k_) =>
                    val (index, distance) = distances(p_)

                    val newPos   = pos.updated(index, p_)
                    val newKeys  = keyLocs - p_
                    val newDoors = doorLocs.filterNot(_._2 == k_.door)
                    State(grid, newKeys, newDoors, newPos) -> distance
            }.toMap
        }

        override def toString = pos.toString + " - " + keyLocs.toString
    }

    case class Point(x: Int, y: Int) {
        def neighbours(g: Grid, k: KeyMap, d: DoorMap): Iterable[Point] =
            if (k.contains(this)) List.empty
            else
                g.nonDiagNeighbours(x, y, false)
                 .map(n => Point(n._1, n._2))
                 .filter(p => g.get(p.x, p.y))
                 .filterNot(d.contains(_))

        override def toString = "(" + x + "," + y + ")"
    }

    def parseInput(lines: List[String]): State = {
        val height = lines.size
        val width  = lines(0).size

        val g = GridUtils.from2DCharArray(lines, '#')
        // We have to invert it because # is walls, and . is not the
        // complete set of open spaces due to the other objects in
        // the map
        val walls = g.grid.map(!_)
        val grid = new Grid(walls, width, height)

        @tailrec
        def parseObjects
            ( x: Int
            , y: Int
            , keys: KeyMap
            , doors: DoorMap
            , pos: List[Point])
            : (KeyMap, DoorMap, List[Point]) = {

            if (x == width - 1 && y == height - 1) (keys, doors, pos)
            else {
                val ch = lines(y)(x)
                val x_ = if (x == width - 1) 0 else x + 1
                val y_ = if (x == width - 1) y + 1 else y
                ch match {
                    case c if c.isLetter && c.isLower => parseObjects(
                        x_, y_, keys.updated(Point(x, y), Key(c)), doors, pos
                    )
                    case c if c.isLetter && c.isUpper => parseObjects(
                        x_, y_, keys, doors.updated(Point(x, y), Door(c)), pos
                    )
                    case c if c == '@' => parseObjects(
                        x_, y_, keys, doors, Point(x, y) :: pos
                    )
                    case _ => parseObjects(x_, y_, keys, doors, pos)
                }
            }
        }

        val (keys, doors, pos) =
            parseObjects(0, 0, Map.empty, Map.empty, List.empty)

        State(grid, keys, doors, pos)
    }

    type DoorMap = Map[Point, Door]
    type KeyMap = Map[Point, Key]

    case class Key(symbol: Char)  extends Object {
        def door = Door(symbol.toUpper)
    }
    case class Door(symbol: Char)
}
