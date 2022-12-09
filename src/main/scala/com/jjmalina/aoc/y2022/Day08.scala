package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import cats.conversions.all

object Day08 extends AOCApp(2022, 8) {
  type Position = (Int, Int)

  def adjacentTop(forest: Vector[Vector[Int]], position: Position): List[Position] =
    if (position._1 == 0) List() else (position._1 -1 to 0 by -1).map((_, position._2)).toList

  def adjacentRight(forest: Vector[Vector[Int]], position: Position): List[Position] = {
    val lastColumnIndex = forest.head.length - 1
    if (position._2 == lastColumnIndex)
      List() else (position._2 + 1 to lastColumnIndex).map((position._1, _)).toList
  }

  def adjacentBottom(forest: Vector[Vector[Int]], position: Position): List[Position] = {
    val lastRowIndex = forest.length - 1
    if (position._1 == lastRowIndex)
      List() else (position._1 + 1 to lastRowIndex).map((_, position._2)).toList
  }

  def adjacentLeft(forest: Vector[Vector[Int]], position: Position): List[Position] =
    if (position._2 == 0) List() else (position._2 - 1 to 0 by -1 ).map((position._1, _)).toList

  def isVisible(forest: Vector[Vector[Int]], position: Position): Boolean = {
    val positionHeight = forest(position._1)(position._2)
    List(
      adjacentTop(forest, position).map(p => forest(p._1)(p._2)).forall(_ < positionHeight),
      adjacentRight(forest, position).map(p => forest(p._1)(p._2)).forall(_ < positionHeight),
      adjacentBottom(forest, position).map(p => forest(p._1)(p._2)).forall(_ < positionHeight),
      adjacentLeft(forest, position).map(p => forest(p._1)(p._2)).forall(_ < positionHeight)
    ).exists(identity)
  }

  def countUntilBlocked(heights: List[Int], height: Int): Int = {
    val lessThan = heights.takeWhile(_ < height).length
    if (heights.length == lessThan) lessThan else lessThan + 1
  }

  def scenicScore(forest: Vector[Vector[Int]], position: Position): Int = {
    val positionHeight = forest(position._1)(position._2)
    List(
      countUntilBlocked(adjacentTop(forest, position).map(p => forest(p._1)(p._2)), positionHeight),
      countUntilBlocked(adjacentRight(forest, position).map(p => forest(p._1)(p._2)), positionHeight),
      countUntilBlocked(adjacentBottom(forest, position).map(p => forest(p._1)(p._2)), positionHeight),
      countUntilBlocked(adjacentLeft(forest, position).map(p => forest(p._1)(p._2)), positionHeight),
    ).foldRight(1)(_ * _)
  }

  def findVisibleTreePositions(forest: Vector[Vector[Int]]): List[Position] = {
    (for {
      row <- (0 until forest.length)
      column <- (0 until forest.head.length)
      if (isVisible(forest, (row, column)))
    } yield (row, column)).toList
  }

  def solve(input: Stream[IO, String], f: (Vector[Vector[Int]]) => String): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .map(_.toVector.map(_.asDigit))
      .compile
      .toVector
      .map(f(_))
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, forest => findVisibleTreePositions(forest).length.toString)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, forest => {
      val treeScores = findVisibleTreePositions(forest)
        .map(pos => (pos, scenicScore(forest, pos)))
        .sortBy(row => -row._2)
      treeScores.head._2.toString
    })

  def runExamples: Unit = {
    val input = List(
      "30373",
      "25512",
      "65332",
      "33549",
      "35390",
    )
    val trees: Vector[Vector[Int]] = input.map(_.toVector.map(_.asDigit)).toVector
    val visibleTreePositions = findVisibleTreePositions(trees)
    println(visibleTreePositions.length)
    val treeScores = visibleTreePositions
      .map(pos => (pos, scenicScore(trees, pos)))
      .sortBy(row => -row._2)
    println(treeScores.head)
  }
}
