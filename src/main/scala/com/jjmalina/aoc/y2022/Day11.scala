package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day11 extends AOCApp(2022, 11) {
  case class Monkey(
    id: Int,
    items: List[Int],
    operation: Int => Int,
    testDivisibleBy: Int,
    testPassMonkey: Int,
    testFailMonkey: Int,
  )

  def parseOperation(s: String): Option[Int => Int] = {
    s match {
      case s"new = old $operator $operand" => {
        operand.toIntOption.map(number => {
          operator match {
            case "+" => (i: Int) => i + number
            case "-" => (i: Int) => i - number
            case "*" => (i: Int) => i * number
            case "/" => (i: Int) => i / number
          }
        })
      }
      case _ => None
    }
  }

  def parseMonkeyLines(lines: List[String]): Option[Monkey] = {
    val initial: (
      Option[Int],
      Option[List[Int]],
      Option[Int => Int],
      Option[Int],
      Option[Int],
      Option[Int]
    ) = (None, None, None, None, None, None)
    lines.foldLeft(initial)((acc, line) => {
      line.trim match {
        case s"Monkey $id" => (id.toIntOption, acc._2, acc._3, acc._4, acc._5, acc._6)
        case s"Starting items: $items" => (
          acc._1, Some(items.split(", ").map(_.toInt).toList),
          acc._3, acc._4, acc._5, acc._6
        )
        case s"Operation: $operation" => (
          (acc._1, acc._2, parseOperation(operation), acc._4, acc._5, acc._6)
        )
        case s"Test: divisible by $divisibleBy" => (
          (acc._1, acc._2, acc._3, divisibleBy.toIntOption, acc._5, acc._6)
        )
        case s"If true: throw to monkey $monkey" => (
          (acc._1, acc._2, acc._3, acc._4, monkey.toIntOption, acc._6)
        )
        case s"If false: throw to monkey $monkey" => (
          (acc._1, acc._2, acc._3, acc._4, acc._5, monkey.toIntOption)
        )
        case _ => initial
      }
    }).mapN(Monkey.apply)
  }

  def solve(input: Stream[IO, String]): IO[String] = {
    ???
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    ???

  override def part2(input: Stream[IO, String]): IO[String] =
    ???

  import scala.io.Source

  def runExamples: Unit = {
    val data = Source.fromFile("notes/day11/example.txt").getLines.toList
    val monks = data.foldLeft(List.empty[List[String]])((acc, line) => {
      line match {
        case "" => List.empty[String] :: acc
        case (s: String) => acc match {
          case head :: rest => (s :: head) :: rest
          case Nil => List(List(s))
        }
      }
    }).map(lines => parseMonkeyLines(lines))
    println(monks)
  }
}
