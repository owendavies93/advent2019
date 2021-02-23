package advent2019

import scalaadventutils.Dijkstra
import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

object Day6 {

    type Graph = Map[String, Map[String, Int]]

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day6")
        println(countOrbits(lines))
        println(findPath(lines))
    }

    def countOrbits(lines: List[String]): Int = {
        val g = parseInput(lines)
        val graph = new WeightedUndirectedGraph[String](g)
        graph.keys.toList.map(k => {
            val ks = graph.traverseFrom(k).nodes - k
            ks.size
        }).sum
    }

    def findPath(lines: List[String]): Int = {
        val g = parseGraph(lines)
        val graph = new WeightedUndirectedGraph[String](g)
        val path = Dijkstra.shortestPath(graph, "YOU", "SAN")
        path.size - 3
    }

    def parseInput(lines: List[String]): Graph =
        lines.map(l => {
            val s = l.split(')')
            s(1) -> Map(s(0) -> 1)
        }).toMap

    def parseGraph(lines: List[String]): Graph =
        lines.map(l => {
            val s = l.split(')')
            List((s(1) -> Map(s(0) -> 1)), s(0) -> Map(s(1) -> 0))
        }).flatten
          .groupBy(_._1)
          .view
          .mapValues(_.map(_._2).reduce(_ ++ _)).toMap
}
