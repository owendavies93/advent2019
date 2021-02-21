package advent2019

import scalaadventutils.Problem

import annotation.tailrec

object Day1 {

    def main(args: Array[String]): Unit = {
        val masses = Problem.parseInputToList("day1").map(_.toInt)
        println(sumFuels(masses))
        println(sumFuelDowns(masses))
    }

    def sumFuels(masses: List[Int]) = masses.map(getFuel).sum

    def sumFuelDowns(masses: List[Int]) = masses.map(getFuelDown(_, 0)).sum

    def getFuel(mass: Int) = (mass / 3) - 2

    @tailrec
    def getFuelDown(mass: Int, fuel: Int): Int = {
        if (mass <= 6) fuel
        else {
            val newFuel = getFuel(mass)
            getFuelDown(newFuel, fuel + newFuel)
        }
    }
}
