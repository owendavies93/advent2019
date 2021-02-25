package advent2019

import scalaadventutils.Problem

object Day8 {

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day8")
        println(part1(input, 6, 25))
        println(part2(input, 6, 25))
    }

    def part1(input: String, height: Int, width: Int) = {
        val fewestZeros = parseInput(input, height, width).minBy(_.count('0'))
        fewestZeros.count('1') * fewestZeros.count('2')
    }

    def part2(input: String, height: Int, width: Int) = {
        val layers = parseInput(input, height, width).map(_.data)
        val image = layers.transpose.map(_.find(p => p != '2').get).toArray
        (0 until height).map(y =>
            (0 until width).map(x =>
                if (image(y * width + x) == '1') '#' else '.'
            ).mkString
        ).mkString("\n")
    }

    def parseInput(input: String, height: Int, width: Int): List[Layer] =
        input.grouped(height * width).map(l => Layer(l, width)).toList

    case class Layer(data: String, width: Int) {
        def count(ch: Char) = data.count(_ == ch)
    }
}
