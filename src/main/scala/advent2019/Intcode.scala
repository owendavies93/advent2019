package advent2019

import annotation.tailrec
import scala.io.StdIn.readLine

object Intcode {

    type PosMap = Map[Int, Int]
    type PModes = (Int, Int, Int)

    case class State(pm: PosMap, in: List[Int], out: List[Int]) {
        def read = in match {
            case head :: tail => (head, copy(in = tail))
            case Nil          => throw new EmptyInputException
        }

        def set(r: Int, v: Int) = copy(pm = pm.updated(r, v))

        def write(i: Int) = copy(out = out :+ i)
    }

    def run
        ( program: String
        , input: List[Int] = List.empty
        , overrides: PosMap = Map.empty)
        : (PosMap, List[Int])= {

        val posmap = parseInput(program)
        val withOverrides = overrides.keys.foldLeft(posmap)((next, k) =>
            next.updated(k, overrides(k))
        )

        @tailrec
        def step(ptr: Int, st: State): (PosMap, List[Int]) = {
            val (opCode, pmodes) = parseOpCode(st.pm(ptr))
            if (opCode == 99) (st.pm, st.out)
            else {
                val (ptr_, st_) = op(opCode, ptr, pmodes, st)
                step(ptr_, st_)
            }
        }

        step(0, State(withOverrides, input, List()))
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
        , state: State)
        : (Int, State) = opCode match {

        // Addition
        case 1 =>
            val left  = modeVal(pmodes._1, ptr + 1, state.pm)
            val right = modeVal(pmodes._2, ptr + 2, state.pm)
            val resP  = state.pm(ptr + 3)
            (ptr + 4, state.set(resP, left + right))

        // Multiplication
        case 2 =>
            val left  = modeVal(pmodes._1, ptr + 1, state.pm)
            val right = modeVal(pmodes._2, ptr + 2, state.pm)
            val resP  = state.pm(ptr + 3)
            (ptr + 4, state.set(resP, left * right))

        // Input
        case 3 =>
            val (input, newState) = state.read
            val resP  = newState.pm(ptr + 1)
            (ptr + 2, newState.set(resP, input.toInt))

        // Output
        case 4 =>
            val output = modeVal(pmodes._1, ptr + 1, state.pm)
            (ptr + 2, state.write(output))

        // Jump If True
        case 5 =>
            val test = modeVal(pmodes._1, ptr + 1, state.pm)
            if (test != 0) {
                val newPtr = modeVal(pmodes._2, ptr + 2, state.pm)
                (newPtr, state)
            } else (ptr + 3, state)

        // Jump If False
        case 6 =>
            val test = modeVal(pmodes._1, ptr + 1, state.pm)
            if (test == 0) {
                val newPtr = modeVal(pmodes._2, ptr + 2, state.pm)
                (newPtr, state)
            } else (ptr + 3, state)

        // Less Than
        case 7 =>
            val left  = modeVal(pmodes._1, ptr + 1, state.pm)
            val right = modeVal(pmodes._2, ptr + 2, state.pm)
            val resP  = state.pm(ptr + 3)
            (ptr + 4, state.set(resP, if (left < right) 1 else 0))

        // Equals
        case 8 =>
            val left  = modeVal(pmodes._1, ptr + 1, state.pm)
            val right = modeVal(pmodes._2, ptr + 2, state.pm)
            val resP  = state.pm(ptr + 3)
            (ptr + 4, state.set(resP, if (left == right) 1 else 0))

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

    final case class EmptyInputException
        ( private val message: String = ""
        , private val cause: Throwable = None.orNull
        ) extends Exception(message, cause)
}
