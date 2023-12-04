package com.jjmalina.aoc.y2023

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day03 extends AOCApp(2023, 3) {
  def part1(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_ != "\n")
      .compile
      .toList
      .map(parseSchematic(_).partNumbers.map(_.value).sumAll.toString)

  def part2(input: Stream[IO, String]): IO[String] =
     input
      .through(text.lines)
      .filter(_ != "\n")
      .compile
      .toList
      .map(parseSchematic(_).gearRatio.toString)

  case class Position(row: Int, col: Int) {
    def adjacentPositions: Set[Position] =
      Set(
        Position(row - 1, col -1),
        Position(row - 1, col),
        Position(row - 1, col + 1),
        Position(row, col - 1),
        Position(row, col + 1),
        Position(row + 1, col -1),
        Position(row + 1, col),
        Position(row + 1, col + 1),
      )

    def isAdjacent(other: Position): Boolean = adjacentPositions.contains(other)
  }
  case class Symbol(value: Char, position: Position)
  case class Number(digits: List[Symbol]) {
    def value: Int = digits.map(_.value).mkString.toInt
    def isPartNumber(symbols: List[Symbol]): Boolean =
      digits.exists(d => symbols.exists(sym => d.position.isAdjacent(sym.position)))
  }
  case class Schematic(numbers: List[Number] = List.empty[Number], symbols: List[Symbol] = List.empty[Symbol]) {
    def digits: List[Symbol] = numbers.flatMap(_.digits)
    def partNumbers: List[Number] = numbers.filter(n => n.isPartNumber(symbols))
    def notPartNumbers: List[Number] = numbers.filter(n => n.isPartNumber(symbols))
    def combine(that: Schematic) = {
      Schematic(this.numbers ++ that.numbers, this.symbols ++ that.symbols)
    }
    def gearRatio: Int = {
      val allDigits = digits
      val gearSymbols = symbols.filter(_.value == '*')
      gearSymbols.flatMap(symbol => {
        val nearbyNumbers = numbers.filter(
          n => (n.digits.head.position.row == symbol.position.row - 1 ||
               n.digits.head.position.row == symbol.position.row ||
               n.digits.head.position.row == symbol.position.row + 1)
        )
        val adjacentNumbers = nearbyNumbers.filter(n => n.digits.exists(_.position.isAdjacent(symbol.position)))
        if (adjacentNumbers.length == 2) {
          Some(adjacentNumbers.map(_.value).reduce(_ * _))
        } else None
      }).sumAll
    }
  }

  def parseLine(row: Int, line: String): Schematic = {
    val length = line.length
    val (schematic, _) = line.toList.zipWithIndex.foldLeft((Schematic(), Option.empty[Number])) {
      case ((currentSchematic, Some(number)), (char, col)) => {
        if (char.isDigit) {
          if (col + 1 == length) {
            (
              currentSchematic.copy(
                numbers=currentSchematic.numbers :+ number.copy(
                  digits=number.digits :+ Symbol(char, Position(row, col))
                )
              ),
              None
            )
          } else {
            (currentSchematic, Some(number.copy(digits=number.digits :+ Symbol(char, Position(row, col)))))
          }
        } else if (char == '.') {
          (currentSchematic.copy(numbers=currentSchematic.numbers :+ number), None)
        } else {
          (
            currentSchematic.copy(
              numbers=currentSchematic.numbers :+ number,
              symbols=currentSchematic.symbols :+ Symbol(char, Position(row, col))
            ),
            None
          )
        }
      }
      case ((currentSchematic, None), (char, col)) => {
        if (char.isDigit) {
          (currentSchematic, Some(Number(digits=List(Symbol(char, Position(row, col))))))
        } else if (char == '.') {
          (currentSchematic, None)
        } else {
          (currentSchematic.copy(symbols=currentSchematic.symbols :+ Symbol(char, Position(row, col))), None)
        }
      }
    }
    schematic
  }

  def parseSchematic(lines: List[String]): Schematic =
    lines.zipWithIndex.foldRight(Schematic()) {
      case ((line, row), currentSchematic) => currentSchematic.combine(parseLine(row, line))
    }

  val sampleInput = List(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598..",
  )

  def runOnSample: Unit = {
    val schematic = parseSchematic(sampleInput)
    println(schematic.partNumbers.map(_.value).sumAll)
  }

  def runOnSample2: Unit = {
    val schematic = parseSchematic(sampleInput)
    println(schematic.gearRatio)
  }
}
