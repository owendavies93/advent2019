package advent2019

import annotation.tailrec
import scala.io.StdIn.readLine

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
            val (opCode, pmodes) = parseOpCode(pm(ptr))
            if (opCode == 99) pm
            else {
                val (ptr_, pm_) = op(opCode, ptr, pmodes, pm)
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
        , pmodes: PModes
        , posMap: PosMap)
        : (Int, PosMap) = opCode match {

        // Addition
        case 1 =>
            val left  = modeVal(pmodes._1, ptr + 1, posMap)
            val right = modeVal(pmodes._2, ptr + 2, posMap)
            val resP  = posMap(ptr + 3)
            val res   = posMap.updated(resP, left + right)
            (ptr + 4, res)

        // Multiplication
        case 2 =>
            val left  = modeVal(pmodes._1, ptr + 1, posMap)
            val right = modeVal(pmodes._2, ptr + 2, posMap)
            val resP  = posMap(ptr + 3)
            val res   = posMap.updated(resP, left * right)
            (ptr + 4, res)

        // Input
        case 3 =>
            // XXX: workaround for annoying sbt bug
            readLine()
            val input = readLine()
            val resP  = posMap(ptr + 1)
            val res   = posMap.updated(resP, input.toInt)
            (ptr + 2, res)

        // Output
        case 4 =>
            val output = modeVal(pmodes._1, ptr + 1, posMap)
            println(output)
            (ptr + 2, posMap)

        // Jump If True
        case 5 =>
            val test = modeVal(pmodes._1, ptr + 1, posMap)
            if (test != 0) {
                val newPtr = modeVal(pmodes._2, ptr + 2, posMap)
                (newPtr, posMap)
            } else (ptr + 3, posMap)

        // Jump If False
        case 6 =>
            val test = modeVal(pmodes._1, ptr + 1, posMap)
            println(test)
            if (test == 0) {
                val newPtr = modeVal(pmodes._2, ptr + 2, posMap)
                (newPtr, posMap)
            } else (ptr + 3, posMap)

        // Less Than
        case 7 =>
            val left  = modeVal(pmodes._1, ptr + 1, posMap)
            val right = modeVal(pmodes._2, ptr + 2, posMap)
            val resP  = posMap(ptr + 3)
            val res   = posMap.updated(resP, if (left < right) 1 else 0)
            (ptr + 4, res)

        // Equals
        case 8 =>
            val left  = modeVal(pmodes._1, ptr + 1, posMap)
            val right = modeVal(pmodes._2, ptr + 2, posMap)
            val resP  = posMap(ptr + 3)
            val res   = posMap.updated(resP, if (left == right) 1 else 0)
            (ptr + 4, res)

        case _ => throw NoSuchOpCodeException(opCode.toString)
    }

    def modeVal(mode: Int, posVal: Int, posMap: PosMap) =
        // Position Mode
        if (mode == 0) posMap(posMap(posVal))
        // Immeadiate Mode
        else if (mode == 1) posMap(posVal)
        else throw NoSuchOpCodeException("Impossible mode: " + mode)

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
