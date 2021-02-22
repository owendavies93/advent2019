package advent2019

import annotation.tailrec

object Day4 {

    val repeat = """(\d)\1""".r

    def main(args: Array[String]): Unit = {
        println(part1(356261, 846303))
        println(part2(356261, 846303))
    }

    def part1(from: Int, to: Int) = {
        (from to to).count(i => {
            val str = i.toString
            repeat.findFirstIn(str).isDefined &&
            str.sliding(2).forall(l => l(0) <= l(1))
        })
    }

    def part2(from: Int, to: Int) = {
        (from to to).count(i => {
            val str = i.toString
            checkIsolatedPair(str, 0) &&
            str.sliding(2).forall(l => l(0) <= l(1))
        })
    }

    @tailrec
    def checkIsolatedPair(str: String, i: Int): Boolean = {
        if (i >= str.length - 1) false
        else if (
            (str(i) == str(i + 1)) &&
            (i == 0 || str(i) != str(i - 1)) &&
            (i == str.length - 2 || str(i + 1) != str(i + 2))
        ) true
        else checkIsolatedPair(str, i + 1)
    }
}
