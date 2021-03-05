package advent2019

import scalaadventutils.Problem

import annotation.tailrec

object Day23 {

    type Nics = Map[Long, Intcode.State]

    def main(args: Array[String]): Unit = {
        val input = Problem.parseInputToString("day23")
        println(part1(input))
        //println(part2(input))
    }

    // Very grim but I haven't yet figured out how to create a neat
    // tail recursive equivalent without passing a huge amount of
    // parameters around
    def part1(input: String): Long = {
        var nics = getNics(50L, input)

        while (true) {
            for (i <- 0L until 50L) {
                var waiting = nics(i)

                if (waiting.in.isEmpty && waiting.out.isEmpty) {
                    val withInput = Intcode.restart(waiting, List(-1L))
                    nics = nics.updated(i, withInput)
                } else {
                    while (waiting.out.size >= 3) {
                        val packet = waiting.out.take(3)
                        val rest = waiting.out.drop(3)

                        val dest = packet.head
                        if (dest == 255) return packet(2)
                        else {
                            val x = packet(1)
                            val y = packet(2)

                            val withInput =
                                nics(dest).restart(nics(dest).in ++ List(x, y))
                            nics = nics.updated(dest, withInput)
                            waiting = waiting.copy(out = rest)

                        }
                    }
                    nics = nics.updated(i, waiting)
                }
            }
        }
        -1L
    }

    // TODO: reduce duplication
    def part2(input: String): Long = {
        var nics = getNics(50L, input)

        // This makes assumptions about the results we'll get, and
        // is generally horrible
        var natx = Long.MaxValue
        var naty = Long.MaxValue
        var prev = -1L

        while (true) {

            if (isIdle(nics)) {
                if (prev != -1 && prev == naty)
                    return prev
                else {
                    prev = naty
                    val withNat =
                        nics(0).restart(nics(0).in ++ List(natx, naty))
                    nics = nics.updated(0, withNat)
                }
            }

            for (i <- 0L until 50L) {
                var waiting = nics(i)

                if (waiting.in.isEmpty && waiting.out.isEmpty) {
                    val withInput = Intcode.restart(waiting, List(-1L))
                    nics = nics.updated(i, withInput)
                } else {
                    while (waiting.out.size >= 3) {
                        val packet = waiting.out.take(3)
                        val rest = waiting.out.drop(3)

                        val dest = packet.head
                        val x = packet(1)
                        val y = packet(2)

                        if (dest == 255) {
                            natx = x
                            naty = y
                        }
                        else {
                            val withInput =
                                nics(dest).restart(nics(dest).in ++ List(x, y))
                            nics = nics.updated(dest, withInput)
                            waiting = waiting.copy(out = rest)

                        }
                    }
                    nics = nics.updated(i, waiting)
                }
            }
        }
        -1L
    }

    private def isIdle(nics: Nics): Boolean =
        nics.values.forall(n => n.in.isEmpty)

    private def getNics(num: Long, input: String): Nics =
        (0L until num).map(i =>
            i -> Intcode.run(input, List(i))
        ).toMap

}
