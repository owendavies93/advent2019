package advent2019

import annotation.tailrec
import scala.io.StdIn.readLine

object Intcode {

    type PosMap = Map[Int, Int]
    type PModes = (Int, Int, Int)

    case class State
        ( pm: PosMap
        , in: List[Int]
        , out: List[Int] = List()
        , ptr: Int = 0
        , waiting: Boolean = false
        , halted: Boolean = false) {

        def halt = copy(halted = true)

        def move(p: Int) = copy(ptr = p)

        def read = in match {
            case head :: tail => (Some(head), copy(in = tail))
            case Nil          => (None, copy(waiting = true))
        }

        def restart(input: List[Int]) = copy(waiting = false, in = input)

        def set(r: Int, v: Int) = copy(pm = pm.updated(r, v))

        def write(i: Int) = copy(out = out :+ i)
    }

    def run
        ( program: String
        , input: List[Int] = List.empty
        , overrides: PosMap = Map.empty)
        : State = {

        val posmap = parseInput(program)
        val withOverrides = overrides.keys.foldLeft(posmap)((next, k) =>
            next.updated(k, overrides(k))
        )

        step(State(withOverrides, input, List()))
    }

    @tailrec
    def step(st: State): State = {
        if (st.waiting) st
        else {
            val (opCode, pmodes) = parseOpCode(st.pm(st.ptr))
            if (opCode == 99) st.halt
            else step(op(opCode, pmodes, st))
        }
    }

    def parseOpCode(opCode: Int): (Int, PModes) = {
        val op = opCode % 100
        val modes = "%03d".format(opCode / 100)
                          .map(_.toInt - 48)
        (op, (modes(2), modes(1), modes(0)))
    }

    def op
        ( opCode: Int
        , pmodes: PModes
        , state: State)
        : State = opCode match {

        // Addition
        case 1 =>
            val left  = modeVal(pmodes._1, state.ptr + 1, state.pm)
            val right = modeVal(pmodes._2, state.ptr + 2, state.pm)
            val resP  = state.pm(state.ptr + 3)
            state.set(resP, left + right).move(state.ptr + 4)

        // Multiplication
        case 2 =>
            val left  = modeVal(pmodes._1, state.ptr + 1, state.pm)
            val right = modeVal(pmodes._2, state.ptr + 2, state.pm)
            val resP  = state.pm(state.ptr + 3)
            state.set(resP, left * right).move(state.ptr + 4)

        // Input
        case 3 =>
            state.read match {
                case (None, s)    => s
                case (Some(i), s) =>
                    val resP = s.pm(s.ptr + 1)
                    s.set(resP, i.toInt).move(s.ptr + 2)
            }

        // Output
        case 4 =>
            val output = modeVal(pmodes._1, state.ptr + 1, state.pm)
            state.write(output).move(state.ptr + 2)

        // Jump If True
        case 5 =>
            val test = modeVal(pmodes._1, state.ptr + 1, state.pm)
            if (test != 0) {
                val newPtr = modeVal(pmodes._2, state.ptr + 2, state.pm)
                state.move(newPtr)
            } else state.move(state.ptr + 3)

        // Jump If False
        case 6 =>
            val test = modeVal(pmodes._1, state.ptr + 1, state.pm)
            if (test == 0) {
                val newPtr = modeVal(pmodes._2, state.ptr + 2, state.pm)
                state.move(newPtr)
            } else state.move(state.ptr + 3)

        // Less Than
        case 7 =>
            val left  = modeVal(pmodes._1, state.ptr + 1, state.pm)
            val right = modeVal(pmodes._2, state.ptr + 2, state.pm)
            val resP  = state.pm(state.ptr + 3)
            state.set(resP, if (left < right) 1 else 0).move(state.ptr + 4)

        // Equals
        case 8 =>
            val left  = modeVal(pmodes._1, state.ptr + 1, state.pm)
            val right = modeVal(pmodes._2, state.ptr + 2, state.pm)
            val resP  = state.pm(state.ptr + 3)
            state.set(resP, if (left == right) 1 else 0).move(state.ptr + 4)

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
