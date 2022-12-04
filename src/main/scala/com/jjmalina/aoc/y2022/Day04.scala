package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day04 extends AOCApp(2022, 4) {
  type Assignment = (Int, Int)

  def fullyContains(left: Assignment, right: Assignment): Boolean = {
    (left._1 <= right._1 && left._2 >= right._2) ||
      (right._1 <= left._1 && right._2 >= left._2)
  }

  def overlaps(left: Assignment, right: Assignment): Boolean =
    (left._1 to left._2).toSet.intersect((right._1 to right._2).toSet) != Set.empty[Int]

  def parseAssignment(s: String): Assignment =
    s.split("-").map(_.toInt).toList match {
      case List(l, r) => (l, r)
      case _ => throw new Exception(s"cannot parse $s")
    }

  def parseLine(s: String): (Assignment, Assignment) = {
    val (left, right) = s.split(",").toList match {
      case List(l, r) => (l, r)
      case _ => throw new Exception(s"cannot parse $s")
    }
    (parseAssignment(left), parseAssignment(right))
  }

  def solve(
    input: Stream[IO, String],
    f: (Assignment, Assignment) => Boolean
  ): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .map(parseLine(_))
      .filter(lr => f(lr._1, lr._2))
      .map(_ => 1)
      .fold(0)(_ + _)
      .compile
      .toList
      .map(_.head.toString)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
   solve(input, fullyContains)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, overlaps)

  val examples = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8",
  )
  val result = examples.map(parseLine(_)).map((lr) => fullyContains(lr._1, lr._2))
}
