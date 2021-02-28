package advent2019

import scalaadventutils.Problem

object Day14 {

    type Chems    = Map[String, Long]
    type Mappings = Map[String, (Long, Chems)]

    def main(args: Array[String]): Unit = {
        val lines = Problem.parseInputToList("day14")
        println(part1(lines))
        part2(lines)
    }

    def part1(lines: List[String]): Long = findOreAmount(parseInput(lines))

    def part2(lines: List[String]): Unit = {
        val mappings = parseInput(lines)
        // Found by binary-search-by-hand. Probably just about quicker than
        // writing the code for it! Only took about 20 goes
        println(find_("FUEL", 1376631, Map().withDefaultValue(0L), mappings)._1)
    }

    def findOreAmount(ms: Mappings): Long =
        find_("FUEL", 1, Map().withDefaultValue(0L), ms)._1

    private def find_
        ( chem: String
        , total: Long
        , leftovers: Chems
        , ms: Mappings)
        : (Long, Chems) =

        chem match {
            case "ORE" => (total, leftovers)
            case c     =>
                val (outNum, in) = ms(chem)
                val needed = if (total < leftovers(chem)) 0
                             else total - leftovers(chem)

                val leftover = if (needed % outNum == 0) 0
                               else outNum - (needed % outNum)

                val numReact = if (leftover == 0) needed / outNum
                               else (needed / outNum) + 1

                val newLO = leftovers.updated(
                    chem, leftovers(chem) - (total - needed))

                val (res, lo) = in.foldLeft((0L, newLO))((next, i) => {
                    val (num, lo_) = next
                    val (chem, cNum) = i

                    val (inRes, inLo) = find_(chem, numReact * cNum, lo_, ms)
                    (num + inRes, inLo)
                })

                val finalLo = lo.updated(chem, lo(chem) + leftover)
                (res, finalLo)
        }

    private def parseInput(lines: List[String]): Mappings = lines.map(l => {
        val sides  = l.split(" => ")
        val output = sides(1).split(" ")
        val inputs = sides(0).split(", ").map(s => {
            val chem = s.split(" ")
            chem(1) -> chem(0).toLong
        }).toMap

        output(1) -> (output(0).toLong -> inputs)
    }).toMap
}
