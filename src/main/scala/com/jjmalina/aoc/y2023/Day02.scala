package com.jjmalina.aoc.y2023

import scala.io.Source

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day02 extends AOCApp(2023, 2) {
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
    val maxDraw = CubeDraw(12, 13, 14)
    solve(
      input,
      s => parseGame(s).filter(_.draws.forall(ds => ds <= maxDraw)).map(_.id).getOrElse(0)
    )

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(
      input,
      s => parseGame(s).map(_.minDraw.power).getOrElse(0)
    )

  case class CubeDraw(red: Int = 0, green: Int = 0, blue: Int = 0) {
    def +(that: CubeDraw): CubeDraw =
      copy(
        red=red + that.red,
        green=green + that.green,
        blue=blue + that.blue
      )

    def <=(that: CubeDraw): Boolean =
      red <= that.red && green <= that.green && blue <= that.blue

    def power: Int = red * green * blue
  }

  case class Game(id: Int, draws: List[CubeDraw]) {
    def minDraw: CubeDraw = {
      draws.foldRight(CubeDraw())((current, currentMin) => {
        CubeDraw(
          if currentMin.red > current.red then currentMin.red else current.red,
          if currentMin.green > current.green then currentMin.green else current.green,
          if currentMin.blue > current.blue then currentMin.blue else current.blue,
        )
      })
    }
  }

  def parseDraw(s: String): Option[CubeDraw] =
    s match {
        case s"$n red" => n.toIntOption.map(number => CubeDraw(red=number))
        case s"$n green" => n.toIntOption.map(number => CubeDraw(green=number))
        case s"$n blue" => n.toIntOption.map(number => CubeDraw(blue=number))
        case _ => None
    }

  def parseDraws(s: String): List[CubeDraw] = {
    s.replaceAll(", ", ",").replaceAll("; ", ";").split(';').toList
      .map(ds => ds.split(",").toList.flatMap(parseDraw).reduce(_ + _))
  }

  def parseGame(s: String): Option[Game] = {
    s match {
      case s"Game $id: $rest" => {
        val draws = parseDraws(rest)
        id.toIntOption.map(Game(_, draws))
      }
      case _ => None
    }
  }

  val sampleInput = List(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
  )

  def runOnSample1: Unit = {
    val maxDraw = CubeDraw(12, 13, 14)
    sampleInput.flatMap(parseGame).filter(_.draws.forall(ds => ds <= maxDraw)).foreach(println)
  }

  def runOnSample2: Unit = {
    println(sampleInput.flatMap(parseGame).map(_.minDraw.power).sumAll)
  }

}
