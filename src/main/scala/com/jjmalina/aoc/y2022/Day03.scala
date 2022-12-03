package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day03 extends AOCApp(2022, 3) {
  val letters = ('a' to 'z').toVector.appendedAll(('A' to 'Z').toVector)

  def bagPriority(s: String): Int = {
    val (left, right) = s.splitAt(s.length / 2)
    val overlap = left.toSet.intersect(right.toSet)
    overlap.map(letters.indexOf(_) + 1).sum
  }

  def groupPriority(ls: List[String]): Int =
    ls
      .map(_.toSet)
      .reduce((l, r) => l.intersect(r))
      .map(letters.indexOf(_) + 1)
      .sum

  def solve(input: Stream[IO, String]): IO[String] = {
    input
      .through(text.lines)
      .map(bagPriority(_))
      .fold(0)(_ + _)
      .compile
      .toList
      .map(_.head.toString)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
   solve(input)

  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .chunkN(3)
      .map(c => groupPriority(c.toList))
      .fold(0)(_ + _)
      .compile
      .toList
      .map(_.head.toString)
}
