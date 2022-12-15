package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import scala.annotation.tailrec

object Day14 extends AOCApp(2022, 14) {
  case class Point(x: Int, y: Int)
  case class Line(points: List[Point]) {
    def drawPoints: Set[Point] =
      points.foldLeft((points.head, Set.empty[Point]))((acc, point) => {
        val (from, linePoints) = acc
        val fromValue = if (from.x == point.x) from.y else from.x
        val toValue = if (from.x == point.x) point.y else point.x
        val byValue = if ((toValue - fromValue) > 0) 1 else -1
        val newPoints = (fromValue to toValue by byValue ).map(xOrY => {
          val x = if (from.x == point.x) from.x else xOrY
          val y = if (from.y == point.y) point.y else xOrY
          Point(x, y)
        }).toSet
        (point, linePoints.union(newPoints))
      })._2
  }

  def parsePoint(s: String): Option[Point] = s match {
    case s"$x,$y" => Some(Point(x.toInt, y.toInt))
    case _ => None
  }

  def parseLine(line: String): Option[Line] =
    line.split(" -> ").toList.map(parsePoint).sequence.map(Line.apply)

  def sandNextPositions(
    sand: Point,
    rocks: Set[Point],
    existingSand: Set[Point],
    floor: Int
  ): List[Point] = {
    List(
      Point(sand.x, sand.y + 1), // down
      Point(sand.x - 1, sand.y + 1), // left
      Point(sand.x + 1, sand.y + 1) // right
    ).filter(p => !rocks.contains(p) && !existingSand.contains(p) && p.y < floor)
  }

  def isPointAtEdgeOfAbyss(point: Point, rocks: Set[Point], existingSand: Set[Point]): Boolean = {
    def pointBelow(p: Point): Boolean = p.x == point.x && p.y > point.y
    !rocks.exists(pointBelow) && !existingSand.exists(pointBelow)
  }

  val initialPoint = Point(500, 0)

  def sandEndingPosition(rocks: Set[Point], existingSand: Set[Point], floor: Int): Point = {
    @tailrec
    def moveSand(p: Point): Point = {
      val nextPoints = sandNextPositions(p, rocks, existingSand, floor)
      if (nextPoints.isEmpty) {
        p
      } else if (floor == Integer.MAX_VALUE && isPointAtEdgeOfAbyss(p, rocks, existingSand)) {
        p
      } else {
        moveSand(nextPoints.head)
      }
    }
    moveSand(initialPoint)
  }

  def fillWithSand(rocks: Set[Point], floor: Int): List[Point] = {
    val initialSand = Set.empty[Point]
    List.unfold((rocks, initialSand))(state => {
      val (currentRocks, currentSand) = state
      val newSand = sandEndingPosition(currentRocks, currentSand, floor)
      if (currentSand.contains(newSand)) {
        None
      } else if (floor == Integer.MAX_VALUE && isPointAtEdgeOfAbyss(newSand, currentRocks, currentSand)) {
        None
      } else {
        Some((newSand, (currentRocks, currentSand + newSand)))
      }
    })
  }

  def fillCaveWithAbyss(lines: List[Line]): List[Point] = {
    val rocks = lines.map(_.drawPoints).reduce(_ union _)
    fillWithSand(rocks, Integer.MAX_VALUE)
  }

  def fillCaveWithFloor(lines: List[Line]): List[Point] = {
    val rocks = lines.map(_.drawPoints).reduce(_ union _)
    val floorY = rocks.maxBy(_.y).y + 2
    fillWithSand(rocks, floorY)
  }

  def solve(input: Stream[IO, String], f: (List[Line]) => List[Point]): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .map(parseLine)
      .compile
      .toList
      .map(
        l => l.sequence.map(f(_).length.toString).getOrElse("Not found")
      )
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, fillCaveWithAbyss)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, fillCaveWithFloor)

  def runExamples: Unit = {
    val inputs = List(
      "498,4 -> 498,6 -> 496,6",
      "503,4 -> 502,4 -> 502,9 -> 494,9",
    )

    val lines = inputs.map(parseLine).sequence.get
    // part 1
    val filledCaveSand = fillCaveWithAbyss(lines)
    println((filledCaveSand, filledCaveSand.length))

    // part 2
    val filledCaveWithFloor = fillCaveWithFloor(lines)
    println((filledCaveWithFloor, filledCaveWithFloor.length))
  }

}
