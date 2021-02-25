package advent2019

import annotation.tailrec
import scala.io.StdIn.readLine

object Intcode {

    type PosMap = Map[Long, Long]
    type PModes = (Int, Int, Int)

    case class State
        ( pm:      PosMap
        , in:      List[Long]
        , out:     List[Long] = List()
        , ptr:     Int       = 0
        , base:    Int       = 0
        , waiting: Boolean   = false
        , halted:  Boolean   = false) {

        def halt = copy(halted = true)

        def move(p: Int) = copy(ptr = p)

        def read = in match {
            case head :: tail => (Some(head), copy(in = tail))
            case Nil          => (None, copy(waiting = true))
        }

        def restart(input: List[Long]) =
            copy(waiting = false, in = input, out = Nil)

        def set(r: Long, v: Long) = copy(pm = pm.updated(r, v))

        def setBase(b: Int) = copy(base = base + b)

        def write(i: Long) = copy(out = out :+ i)
    }

    def run
        ( program: String
        , input: List[Long] = List.empty
        , overrides: PosMap = Map.empty)
        : State = {

        val posmap = parseInput(program)
        val withOverrides = overrides.keys.foldLeft(posmap)((next, k) =>
            next.updated(k, overrides(k))
        ).withDefaultValue(0L)

        step(State(withOverrides, input, List()))
    }

    def restart
        ( program: String
        , state: State
        , input: List[Long] = List.empty)
        : State = {

        val posmap = parseInput(program)
        step(state.restart(input))
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

    def parseOpCode(opCode: Long): (Int, PModes) = {
        val op = (opCode % 100).toInt
        val modes = "%03d".format(opCode / 100)
                          .map(_.toInt - 48)
        (op, (modes(2), modes(1), modes(0)))
    }

    def op
        ( opCode: Int
        , pmodes: PModes
        , st:     State)
        : State = opCode match {

        // Addition
        case 1 =>
            val left  = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            val right = modeVal(pmodes._2, st.ptr + 2, st.pm, st.base)
            val resP  = modeIndex(pmodes._3, st.ptr + 3, st.pm, st.base)
            st.set(resP, left + right).move(st.ptr + 4)

        // Multiplication
        case 2 =>
            val left  = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            val right = modeVal(pmodes._2, st.ptr + 2, st.pm, st.base)
            val resP  = modeIndex(pmodes._3, st.ptr + 3, st.pm, st.base)
            st.set(resP, left * right).move(st.ptr + 4)

        // Input
        case 3 =>
            st.read match {
                case (None, s)    => s
                case (Some(i), s) =>
                    val resP = modeIndex(pmodes._1, s.ptr + 1, st.pm, st.base)
                    s.set(resP, i.toLong).move(s.ptr + 2)
            }

        // Output
        case 4 =>
            val output = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            st.write(output).move(st.ptr + 2)

        // Jump If True
        case 5 =>
            val test = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            if (test != 0) {
                val newPtr = modeVal(pmodes._2, st.ptr + 2, st.pm, st.base)
                st.move(newPtr.toInt)
            } else st.move(st.ptr + 3)

        // Jump If False
        case 6 =>
            val test = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            if (test == 0) {
                val newPtr = modeVal(pmodes._2, st.ptr + 2, st.pm, st.base)
                st.move(newPtr.toInt)
            } else st.move(st.ptr + 3)

        // Less Than
        case 7 =>
            val left  = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            val right = modeVal(pmodes._2, st.ptr + 2, st.pm, st.base)
            val resP  = modeIndex(pmodes._3, st.ptr + 3, st.pm, st.base)
            st.set(resP, if (left < right) 1 else 0).move(st.ptr + 4)

        // Equals
        case 8 =>
            val left  = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            val right = modeVal(pmodes._2, st.ptr + 2, st.pm, st.base)
            val resP  = modeIndex(pmodes._3, st.ptr + 3, st.pm, st.base)
            st.set(resP, if (left == right) 1 else 0).move(st.ptr + 4)

        // Adjust Base
        case 9 =>
            val base = modeVal(pmodes._1, st.ptr + 1, st.pm, st.base)
            st.setBase(base.toInt).move(st.ptr + 2)

        case _ => throw NoSuchOpCodeException(opCode.toString)
    }

    def modeVal(mode: Int, posVal: Long, posMap: PosMap, base: Int) =
        posMap(modeIndex(mode, posVal, posMap, base))

    def modeIndex(mode: Int, index: Long, posMap: PosMap, base: Int) =
        mode match {
            // Position Mode
            case 0 => posMap(index)
            // Immeadiate Mode
            case 1 => index
            // Relative Mode
            case 2 => base + posMap(index)
            case _ => throw NoSuchOpCodeException("Impossible mode: " + mode)
        }

    def parseInput(input: String): PosMap =
        strToLongArray(input)
            .zipWithIndex
            .map { case (p, i) => i.toLong -> p }
            .toMap

    private def strToLongArray(str: String) =
        str.split(",").map(_.toLong).toArray

    final case class NoSuchOpCodeException
        ( private val message: String = ""
        , private val cause: Throwable = None.orNull
        ) extends Exception(message, cause)

    final case class EmptyInputException
        ( private val message: String = ""
        , private val cause: Throwable = None.orNull
        ) extends Exception(message, cause)
}
