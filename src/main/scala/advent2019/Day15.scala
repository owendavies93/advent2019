package advent2019

import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import annotation.tailrec

object Day15 {

    def main(args: Array[String]): Unit = {
        val program = Problem.parseInputToString("day15")
        val graph  = getAreaGraph
        val oxygen = findOxygen(program, graph)
        println(oxygen._2)
        println(findFillTime(program, graph, oxygen._1))
    }

    def findOxygen
        ( program: String
        , g: WeightedUndirectedGraph[Node])
        : (Node, Int) = {

        val start = Point(0, 0)
        val startingState
            = Intcode.State(Intcode.parseInput(program), List.empty)
        val from = Node(start)(Open, startingState)
        val result = g.searchFrom(from, (n: Node) => n.pos == Oxygen)

        result.to.get
    }

    def findFillTime
        ( program: String
        , g: WeightedUndirectedGraph[Node]
        , start: Node)
        : Int = g.traverseFrom(start).distances.values.max

    private def getAreaGraph = {
        // TODO: the graph param isn't even used here, because the BFS logic
        //       only ever considers neighbours and we override it. So it
        //       would be neater to split this out of the graph class, or
        //       alter the graph class such that the prebuilt map was optional
        val g = Map[Node, Map[Node, Int]]()
        new WeightedUndirectedGraph[Node](g) {
            override def neighbours(n: Node) = n.neighbours
        }
    }

    private def toPos(i: Int): Position = i match {
        case 0 => Wall
        case 1 => Open
        case 2 => Oxygen
        case _ => throw new Exception(s"unexpected output $i")
    }

    case class Node(p: Point)(val pos: Position, state: Intcode.State) {
        def neighbours = p.neighbours.map(n => {
            val dir      = p.dirTo(n).id
            val newState = Intcode.restart(state.copy(), List(dir.toLong))
            val newPos   = newState.out(0).toInt
            (newPos, newState, n)
        })
            .filter(n => toPos(n._1) != Wall)
            .map {
                case (pos_, state_, p_) => Node(p_)(toPos(pos_), state_)
            }
    }

    case class Point(x: Int, y: Int) {
        def +(o: Point) = Point(x + o.x, y + o.y)

        def dirTo(o: Point) = List(N, S, E, W).find(this + _.transform == o).get

        def neighbours = List(N, S, E, W).map(this + _.transform)

        override def toString = "(" + x + "," + y + ")"
    }

    sealed trait Position
    case object Wall   extends Position
    case object Open   extends Position
    case object Oxygen extends Position

    // Trying this instead of the normal enumeration
    sealed trait Direction {
        def transform: Point
        def id: Int
    }

    case object N extends Direction {
        def transform = Point(0, 1)
        def id = 1
    }

    case object S extends Direction {
        def transform = Point(0, -1)
        def id = 2
    }

    case object E extends Direction {
        def transform = Point(1, 0)
        def id = 4
    }

    case object W extends Direction {
        def transform = Point(-1, 0)
        def id = 3
    }
}
