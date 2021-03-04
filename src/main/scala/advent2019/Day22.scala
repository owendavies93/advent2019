package advent2019

import scalaadventutils.CircularList
import scalaadventutils.Problem

import scala.collection.immutable.Queue

object Day22 {

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day22")
        println(run(lines, 10007, 2019))
        println(part2(lines, BigInt(119315717514047L), 2020, BigInt(101741582076661L)))
    }

    def run(lines: List[String], length: Int, index: Int): Int = {
        val init = CircularList(length, length, (0 until length).to(Queue))
        val res = lines.foldLeft(init)((cl, l) => l match {
            case cut(times) => cut(cl, times.toInt)
            case dwi(inc)   => dealIncrement(cl, inc.toInt)
            case dreg()     => deal(cl)
        })
        res.queue.indexOf(index)
    }

    def cut(deck: CircularList[Int], times: Int) =
        if (times > 0) deck.rotate(times)
        else if (times < 0) deck.rotate(deck.size + times)
        else deck

    def deal(deck: CircularList[Int]) =
        deck.queue.foldLeft(CircularList[Int](deck.size)())(
            (cl, card) => cl.insertAt(0, card)
        )

    def dealIncrement(deck: CircularList[Int], inc: Int) = {
        val arr =
            (0 until deck.size).foldLeft((Array.fill(deck.size)(-1), 0))({
                (next, i) =>
                    val (cl, index) = next
                    cl(index) = deck.queue(i)
                    (cl, (index + inc) % deck.size)
            })._1

        CircularList(deck.size, deck.size, arr.to(Queue))
    }

    val dwi  = """deal with increment (\d+)""".r
    val dreg = """deal into new stack""".r
    val cut  = """cut (-?\d+)""".r

    /*
        Heavy use of hints required for this fairly complex bit of maths...
        We forget about maintaining the list and only worry about the index
        we're interested in.
        Once we've done one shuffle, we can simulate the rest using a
        geometric series (apparently...)
    */
    def part2
        ( lines: List[String]
        , deckSize: BigInt
        , index: Int
        , times: BigInt)
        : BigInt = {

        val mult   = BigInt(1)
        val offset = BigInt(0)
        val (shuffleMult, shuffleOffest) =
            lines.foldLeft((mult, offset))((next, l) => {
                val (m, o) = next
                l match {
                    case cut(times) =>
                        (m, (o + (times.toInt * m)).mod(deckSize))
                    case dwi(inc) =>
                        val inv = BigInt(inc).modPow(deckSize - 2, deckSize)
                        ((m * inv) % deckSize, o)
                    case dreg() =>
                        val minus = m * -1
                        (minus.mod(deckSize), (o + minus).mod(deckSize))
                }
            })

        val finalMult   = shuffleMult.modPow(times, deckSize)
        val finalOffset = shuffleOffest * (1 - finalMult) *
            ((1 - shuffleMult).mod(deckSize)).modPow(deckSize - 2, deckSize)

        (finalOffset + index * finalMult).mod(deckSize)
    }
}
