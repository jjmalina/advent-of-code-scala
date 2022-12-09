package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import cats.syntax.apply

object Day09 extends AOCApp(2022, 9) {
  enum Direction {
    case Left
    case Right
    case Up
    case Down
  }
  import Direction._
  case class Move(direction: Direction, steps: Int)

  case class Position(x: Int, y: Int)
  case class State(
    headPosition: Position,
    tailPosition: Position,
    previousTailPositions: List[Position]
  )

  def initialStates: State =
    State(Position(0, 0), Position(0, 0), List())

  def nextHeadPosition(head: Position, move: Move): Position =
    move.direction match
      case Direction.Left => head.copy(x=head.x - move.steps)
      case Direction.Right => head.copy(x=head.x + move.steps)
      case Direction.Up => head.copy(y=head.y + move.steps)
      case Direction.Down => head.copy(y=head.y - move.steps)

  def isTailAdjacent(head: Position, tail: Position): Boolean = {
    Set(
      tail, // overlap
      Position(tail.x, tail.y + 1), // up
      Position(tail.x, tail.y - 1), // down
      Position(tail.x + 1, tail.y), // right
      Position(tail.x - 1, tail.y), // left
    ).union(diagonals(tail).toSet).contains(head)
  }

  def nextTailPosition(nextHead: Position, tail: Position, move: Move): Position =
    if (isTailAdjacent(nextHead, tail)) {
      tail
    } else {
      move.direction match
        case Direction.Left => Position(nextHead.x + 1, nextHead.y)
        case Direction.Right => Position(nextHead.x - 1, nextHead.y)
        case Direction.Up => Position(nextHead.x, nextHead.y - 1)
        case Direction.Down => Position(nextHead.x, nextHead.y + 1)
    }

  def diagonals(position: Position): List[Position] =
    List(
      Position(position.x - 1, position.y + 1), // top left
      Position(position.x + 1, position.y + 1), // top right
      Position(position.x - 1, position.y - 1), // bottom left
      Position(position.x + 1, position.y - 1), // bottom right
    )

  def tailPath(currentTail: Position, nextTail: Position): List[Position] =
    if (currentTail == nextTail)
      List(nextTail)
    else {
      if (currentTail.x == nextTail.x) {
        (for {
          y <- (currentTail.y to nextTail.y by (if (currentTail.x > nextTail.x) -1 else 1))
        } yield Position(nextTail.x, y)).toList
      } else if (currentTail.y == nextTail.y) {
        (for {
          x <- (currentTail.x to nextTail.x by (if (currentTail.x > nextTail.x) -1 else 1))
        } yield Position(x, nextTail.y)).toList
      } else {
        // we have to take a diagonal
        // take the one which has the closest x and y
        // TODO this may not work on all quadrants of the grid?
        val diag = diagonals(currentTail)
          .minBy(p => math.abs(nextTail.x - p.x) + math.abs(nextTail.y - p.y))
        List(diag).appendedAll(tailPath(diag, nextTail))
      }
    }

  def applyMove(s: State, move: Move): State = {
    val nextHeadPos = nextHeadPosition(s.headPosition, move)
    val nextTailPos = nextTailPosition(nextHeadPos, s.tailPosition, move)
    State(
      nextHeadPos,
      nextTailPos,
      s.previousTailPositions.appendedAll(tailPath(s.tailPosition, nextTailPos))
    )
  }

  def parseMove(line: String): Move = {
    line match {
      case s"L $steps" => Move(Left, steps.toInt)
      case s"R $steps" => Move(Right, steps.toInt)
      case s"U $steps" => Move(Up, steps.toInt)
      case s"D $steps" => Move(Down, steps.toInt)
      case _ => throw new Exception(s"cant parse $line")
    }
  }

  def solve(input: Stream[IO, String]): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .map(parseMove)
      .fold(initialStates)(applyMove)
      .compile
      .toList
      .map(_.headOption.map(s => {
        println((s.headPosition, s.tailPosition))
        s.previousTailPositions.toSet.size.toString
      }).getOrElse("Failed"))
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input)

  override def part2(input: Stream[IO, String]): IO[String] =
   solve(input)

  def renderPositions(positions: Set[Position], height: Int, width: Int): String = {
    (0 to height)
      .map(
        h => (0 to width).map(
          w => if (w == 0 && h == 0) "s"
               else if (positions.contains(Position(w, h))) "#"
               else "."
        ).mkString("")
      )
      .reverse
      .mkString("\n")
  }

  def runExamples: Unit = {
    val lines = List(
      "R 4",
      "U 4",
      "L 3",
      "D 1",
      "R 4",
      "D 1",
      "L 5",
      "R 2",
    )
    // tail visited 13 positions at least once
    val endState = lines.map(parseMove).foldLeft(initialStates)(applyMove)
    println(endState.previousTailPositions)
    println(endState.previousTailPositions.toSet)
    println(renderPositions(endState.previousTailPositions.toSet, 4, 5))
  }
}
