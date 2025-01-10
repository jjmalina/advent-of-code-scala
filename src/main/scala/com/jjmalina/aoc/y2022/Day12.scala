package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day12 extends AOCApp(2022, 12) {

  def getPositionHeight(grid: Vector[Vector[Char]], position: (Int, Int)): Char =
    val currentPosChar = grid(position._1)(position._2)
    if (currentPosChar == 'S') 'a' else if (currentPosChar == 'E') 'z' else currentPosChar

  def nextValidPositions(grid: Vector[Vector[Char]], currentPosition: (Int, Int)): List[(Int, Int)] =
    val currentPositionHeight = getPositionHeight(grid, currentPosition)
    // top, right, bottom, left
    List(
      if (currentPosition._1 > 0) Some((currentPosition._1 - 1, currentPosition._2)) else None,
      if (currentPosition._2 < grid.head.length - 1) Some(currentPosition._1, currentPosition._2 + 1) else None,
      if (currentPosition._1 < grid.length - 1) Some(currentPosition._1 + 1, currentPosition._2) else None,
      if (currentPosition._2 > 0) Some(currentPosition._1, currentPosition._2 - 1) else None
    ).flatten.filter(pos => canMove(currentPositionHeight, getPositionHeight(grid, (pos._1, pos._2))))

  def canMove(current: Char, dest: Char): Boolean = math.abs(dest - current) <= 1

  def findPosition(grid: Vector[Vector[Char]], char: Char): Option[(Int, Int)] =
    grid.zipWithIndex.map(row => {
      val rowIndex = row._2
      val idx = row._1.indexOf(char) match {
        case -1 => None
        case idx: Int => Some(idx)
      }
      idx.map((rowIndex, _))
    }).filter(_.isDefined).headOption.flatMap(identity)

  def findStartPosition(grid: Vector[Vector[Char]]): Option[(Int, Int)] =
    findPosition(grid, 'S')

  def findEndPosition(grid: Vector[Vector[Char]]): Option[(Int, Int)] =
    findPosition(grid, 'E')

  def parseInput(lines: Iterable[String]): Vector[Vector[Char]] =
    lines.map(_.toVector).toVector

  def traverseGrid(
    grid: Vector[Vector[Char]],
    currentPosition: (Int, Int),
    dest: (Int, Int),
    previousPositions: List[(Int, Int)]
  ): List[List[(Int, Int)]] = {
    if (currentPosition == dest) {
      List(List(dest))
    } else {
      nextValidPositions(grid, currentPosition)
        .filter(!previousPositions.contains(_))
        .flatMap(pos => {
          println((dest._1 - currentPosition._1, dest._2 - currentPosition._2))
          traverseGrid(grid, pos, dest, pos :: previousPositions).map(
            currentPosition :: _
          )
        })
    }
  }

  def findShortestPathToDest(
    grid: Vector[Vector[Char]],
    start: (Int, Int),
    end: (Int, Int),
  ): List[(Int, Int)] = {
    val paths = traverseGrid(grid, start, end, List.empty[(Int, Int)])
    paths.filter(_.reverse.head == end).sortBy(_.length).head
  }

  def findPath(input: List[String]): Option[Int] = {
    val grid = parseInput(input)
    val startAndEnd = for {
      startPosition <- findStartPosition(grid)
      endPosition <- findEndPosition(grid)
    } yield (startPosition, endPosition)

    val result = startAndEnd.map {
      case (start, end) => findShortestPathToDest(grid, start, end)
    }
    println(startAndEnd)
    println(result)
    println(result.map(_.length))
    result.map(_.length - 1)
  }

  def solve(input: Stream[IO, String]): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .compile
      .toList
      .map(lines => findPath(lines).map(_.toString).getOrElse("Not found"))
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input)

  def runExamples: Unit = {
    val example = List(
      "Sabqponm",
      "abcryxxl",
      "accszExk",
      "acctuvwj",
      "abdefghi",
    )
    findPath(example)
    // val grid = parseInput(example)
    // val startAndEnd = for {
    //   startPosition <- findStartPosition(grid)
    //   endPosition <- findEndPosition(grid)
    // } yield (startPosition, endPosition)

    // val result = startAndEnd.map {
    //   case (start, end) => findShortestPathToDest(grid, start, end)
    // }
    // println(startAndEnd)
    // println(result)
    // println(result.map(_.length))
  }
}
