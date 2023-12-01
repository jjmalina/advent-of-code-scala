package com.jjmalina.aoc.y2023

import scala.io.Source

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day01 extends AOCApp(2023, 1) {
  def solve(
    input: Stream[IO, String],
    f: String => Integer
  ): IO[String] = {
    input
      .through(text.lines)
      .map(f)
      .fold(0)(_ + _)
      .compile
      .toList
      .map(_.head.toString)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, getDigit(_).getOrElse(0))

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, s => getDigit(replaceSpelled(s)).getOrElse(0))


  def getDigit(s: String): Option[Int] = {
    val digits = s.toList.filter(_.isDigit)
    val first = digits.take(1)
    val last = digits.takeRight(1)
    (first.headOption, last.headOption) match
      case (Some(firstDigit), Some(lastDigit)) => Some((firstDigit.toString + lastDigit.toString).toInt)
      case (None, Some(lastDigit)) => Some((lastDigit.toString + lastDigit.toString).toInt)
      case (Some(firstDigit), None) => Some((firstDigit.toString + firstDigit.toString).toInt)
      case (None, None) => None
  }

  val spelledDigits = List(
    ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"),
    ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"),
    ("nine", "9"),
  )

  def replaceSpelled(s: String): String = {
    def go(acc: List[String], ss: String): List[String] = {
      if (ss.isEmpty) {
        acc
      } else {
        val first = ss.head
        if (first.isDigit) {
          go(first.toString :: acc, ss.slice(1, ss.length))
        } else {
          spelledDigits.find(spd => ss.startsWith(spd._1)) match {
            case Some((_, digit)) => go(digit :: acc, ss.slice(1, ss.length))
            case None => go(acc, ss.slice(1, ss.length))
          }
        }
      }
    }
    go(List(), s).reverse.mkString
  }

  def startsWithSpelledDigit(s: String): Boolean =
    spelledDigits.exists(spd => s.startsWith(spd._1))

  def runOnTests1: Unit = {
    val inputs = List(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet",
    )
    val digits = inputs.flatMap(getDigit)
    println(digits)
    println(digits.reduce(_ + _))
  }

  def runOnTests2: Unit = {
    val inputs = List(
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen",
    )
    val digits = inputs.map(replaceSpelled).flatMap(getDigit)
    println(digits)
    println(digits.reduce(_ + _))
  }

}
