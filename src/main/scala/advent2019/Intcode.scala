package advent2019

import annotation.tailrec

object Intcode {

    type PosMap = Map[Int, Int]
    type PModes = (Int, Int, Int)

    def run(input: String, overrides: PosMap = Map.empty): PosMap = {
        val posmap = parseInput(input)
        val withOverrides = overrides.keys.foldLeft(posmap)((next, k) =>
            next.updated(k, overrides(k))
        )

        @tailrec
        def step(ptr: Int, pm: PosMap): PosMap = {
            val opCode = pm(ptr)
            if (opCode == 99) pm
            else {
                val (ptr_, pm_) = op(opCode, ptr, pm)
                step(ptr_, pm_)
            }
        }

        step(0, withOverrides)
    }

    def parseOpCode(opCode: Int): (Int, PModes) = {
        val op = opCode % 100
        val modes = "%03d".format(opCode / 100)
                          .map(_.toInt - 48)
        (op, (modes(2), modes(1), modes(0)))
    }

    def op
        ( opCode: Int
        , ptr: Int
        , posMap: PosMap)
        : (Int, PosMap) = opCode match {

        // Addition
        case 1 =>
            val left  = posMap(posMap(ptr + 1))
            val right = posMap(posMap(ptr + 2))
            val resP  = posMap(ptr + 3)
            val res   = posMap.updated(resP, left + right)
            (ptr + 4, res)

        // Multiplication
        case 2 =>
            val left  = posMap(posMap(ptr + 1))
            val right = posMap(posMap(ptr + 2))
            val resP  = posMap(ptr + 3)
            val res   = posMap.updated(resP, left * right)
            (ptr + 4, res)

        case _ => throw NoSuchOpCodeException(opCode.toString)
    }

    def parseInput(input: String): PosMap =
        strToIntArray(input)
            .zipWithIndex
            .map { case (p, i) => i -> p }
            .toMap

    private def strToIntArray(str: String) =
        str.split(",").map(_.toInt).toArray

    final case class NoSuchOpCodeException
        ( private val message: String = ""
        , private val cause: Throwable = None.orNull
        ) extends Exception(message, cause)
}
