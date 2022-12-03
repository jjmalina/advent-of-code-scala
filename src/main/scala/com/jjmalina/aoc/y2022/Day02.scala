package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day02 extends AOCApp(2022, 2) {
  def gameScore(left: Char, right: Char): Int = {
    left match {
      case 'A' => { // 1
        right match {
          case 'X' => 1 + 3 // draw
          case 'Y' => 2 + 6 // win
          case 'Z' => 3 // loss
          case _ => throw new Exception(s"invalid right $right")
        }
      }
      case 'B' => right match {
        case 'X' => 1 // loss
        case 'Y' => 2 + 3 // draw
        case 'Z' => 3 + 6 // win
        case _ => throw new Exception(s"invalid right $right")
      }
      case 'C' => right match {
        case 'X' => 1 + 6 // win
        case 'Y' => 2 // loss
        case 'Z' => 3 + 3 // draw
        case _ => throw new Exception(s"invalid right $right")
      }
      case _ => throw new Exception(s"invalid left $left")
    }
  }

  def gameScore2(left: Char, right: Char): Int = {
    left match {
      case 'A' => { // 1
        right match {
          case 'X' => 3 // scissors
          case 'Y' => 1 + 3 // rock draw
          case 'Z' => 2 + 6 // paper win
          case _ => throw new Exception(s"invalid right $right")
        }
      }
      case 'B' => right match {
        case 'X' => 1 // rock loss
        case 'Y' => 2 + 3 // paper draw
        case 'Z' => 3 + 6 // scissors win
        case _ => throw new Exception(s"invalid right $right")
      }
      case 'C' => right match {
        case 'X' => 2  // paper lose win
        case 'Y' => 3 + 3 // scissors draw
        case 'Z' => 1 + 6 // rock win
        case _ => throw new Exception(s"invalid right $right")
      }
      case _ => throw new Exception(s"invalid left $left")
    }
  }

  def solve(
    input: Stream[IO, String],
    f: (Char, Char) => Int
  ): IO[String] = {
    input
      .through(text.lines)
      .map(_.toList.filter(_ != ' '))
      .filter(_.length != 0)
      .map((chars: List[Char]) => f(chars(0), chars(1)))
      .fold(0)(_ + _)
      .compile
      .toList
      .map(_.head.toString)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
   solve(input, gameScore)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, gameScore2)
}
