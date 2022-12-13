package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import scala.math.BigInt
import scala.collection.immutable.Queue

object Day11 extends AOCApp(2022, 11) {
  case class Monkey(
    id: Int,
    items: Queue[BigInt],
    operationStr: String,
    operation: BigInt => BigInt,
    testDivisibleBy: BigInt,
    testPassMonkey: Int,
    testFailMonkey: Int,
    inspectedCount: BigInt,
  ) {
    override def toString: String = {
      List(
        s"Monkey ${id}:",
        s"  Starting items: ${items.mkString(", ")}",
        s"  Operation: $operationStr",
        s"  Test: divisible by $testDivisibleBy",
        s"    If true: throw to monkey $testPassMonkey",
        s"    if false: throw to monkey $testFailMonkey",
        s""
      ).mkString("\n")
    }
  }

  object Monkey {
    def turn(monkey: Monkey, worryLevelDivisor: BigInt, gcm: Option[BigInt]): (Monkey, Queue[(Int, BigInt)]) = {
      var currentItemCount = 0
      val items = monkey.items.map(item => {
        currentItemCount += 1
        val worryLevel = monkey.operation(item)
        val worry = (worryLevel / worryLevelDivisor)
        val newWorryLevel = gcm.map(worry % _).getOrElse(worry)
        val isDivisible = newWorryLevel % monkey.testDivisibleBy == BigInt("0")
        if (isDivisible)
          (monkey.testPassMonkey, newWorryLevel) else (monkey.testFailMonkey, newWorryLevel)
      })
      val newInspectedCount = monkey.inspectedCount + currentItemCount
      (monkey.copy(items=Queue.empty[BigInt], inspectedCount=newInspectedCount), items)
    }

    def round(monkeys: List[Monkey], worryLevelDivisor: BigInt, gcm: Option[BigInt]): List[Monkey] = {
      monkeys.foldLeft(monkeys.map(m => (m.id, m)).toMap)((monkeyState, monkey) => {
        val currentMonkey = monkeyState(monkey.id)
        val (newMonkey, items) = Monkey.turn(currentMonkey, worryLevelDivisor, gcm)
        val newMonkeyState = monkeyState.updated(monkey.id, newMonkey)
        items.foldLeft(newMonkeyState)((ms, item) => {
          val (id, worryLevel) = item
          val monkeyToUpdate = ms(id)
          val newItems = monkeyToUpdate.items.appended(worryLevel)
          ms.updated(id, monkeyToUpdate.copy(items=newItems))
        })
      }).values.toList.sortBy(_.id)
    }
  }

  def parseOperation(s: String): Option[BigInt => BigInt] = {
    s match {
      case s"new = old $operator $operand" => {
        operand match {
          case "old" => Some(operator match {
            case "+" => (i: BigInt) => i + i
            case "-" => (i: BigInt) => i - i
            case "*" => (i: BigInt) => i * i
            case "/" => (i: BigInt) => i / i
          })
          case _ =>  operand.toIntOption.map(number => {
            operator match {
              case "+" => (i: BigInt) => i + BigInt(number)
              case "-" => (i: BigInt) => i - BigInt(number)
              case "*" => (i: BigInt) => i * BigInt(number)
              case "/" => (i: BigInt) => i / BigInt(number)
            }
          })
        }
      }
      case _ => None
    }
  }

  def parseMonkeyLines(lines: List[String]): Option[Monkey] = {
    val initial: (
      Option[Int],
      Option[Queue[BigInt]],
      Option[String],
      Option[BigInt => BigInt],
      Option[BigInt],
      Option[Int],
      Option[Int],
      Option[BigInt],
    ) = (None, None, None, None, None, None, None, Some(0))
    lines.foldLeft(initial)((acc, line) => {
      line.trim match {
        case s"Monkey $id:" => (id.toIntOption, acc._2, acc._3, acc._4, acc._5, acc._6, acc._7, acc._8)
        case s"Starting items: $items" => (
          acc._1, Some(Queue.from(items.split(", ").map(n => BigInt(n)))),
          acc._3, acc._4, acc._5, acc._6, acc._7, acc._8
        )
        case s"Operation: $operation" => (
          (acc._1, acc._2, Some(operation), parseOperation(operation), acc._5, acc._6, acc._7, acc._8)
        )
        case s"Test: divisible by $divisibleBy" => (
          (acc._1, acc._2, acc._3, acc._4, divisibleBy.toIntOption.map(BigInt(_)), acc._6, acc._7, acc._8)
        )
        case s"If true: throw to monkey $monkey" => (
          (acc._1, acc._2, acc._3, acc._4, acc._5, monkey.toIntOption, acc._7, acc._8)
        )
        case s"If false: throw to monkey $monkey" => (
          (acc._1, acc._2, acc._3, acc._4, acc._5, acc._6, monkey.toIntOption, acc._8)
        )
        case _ => initial
      }
    }).mapN(Monkey.apply)
  }

  def parseLines(lines: List[String]): Option[List[Monkey]] = {
    lines.foldLeft(List.empty[List[String]])((acc, line) => {
      line match {
        case "" => List.empty[String] :: acc
        case (s: String) => acc match {
          case head :: rest => (s :: head) :: rest
          case Nil => List(List(s))
        }
      }
    }).map(lines => parseMonkeyLines(lines)).sequence
  }

  def monkeyBusiness(monkeys: List[Monkey], rounds: Int, worryLevelDivisor: BigInt): BigInt = {
    val gcm = monkeys.map(_.testDivisibleBy).product
    val endMonkeys = (1 to rounds).foldLeft(monkeys.sortBy(_.id))((mm, round) => {
      Monkey.round(mm, worryLevelDivisor, if (worryLevelDivisor == BigInt(1)) Some(gcm) else None)
    })
    endMonkeys.map(_.inspectedCount).sorted.reverse.take(2).reduce(_ * _)
  }

  def solve(input: Stream[IO, String], rounds: Int, worryLevelDivisor: BigInt = BigInt("3")): IO[String] = {
    input
      .through(text.lines)
      .compile
      .toList
      .map(lines => {
        val monks = parseLines(lines.slice(0, lines.length - 1))
        monks.map(monkeys =>
          monkeyBusiness(
            monkeys,
            rounds,
            worryLevelDivisor
          ).toLong.toString
        ).getOrElse("Nope")
      })
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, 20)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, 10000, BigInt(1))

  import scala.io.Source

  def runExamples: Unit = {
    val data = Source.fromFile("notes/day11/example.txt").getLines.toList
    val monks = parseLines(data)
    val result = monks.map(monkeyBusiness(_, 20, BigInt(3)))
    println(result.map(_.toString))
  }
}
