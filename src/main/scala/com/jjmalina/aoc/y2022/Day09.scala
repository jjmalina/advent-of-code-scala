package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

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

  def nextTailPosition(nextHead: Position, tail: Position): Position =
    if (isTailAdjacent(nextHead, tail)) {
      tail
    } else {
      /*
      how do we make this work without the move???
        nextHead.x < tail.x -> left
        nextHead.x > tail.x -> right
        nextHead.y > tail.y -> up
        nextHead.y < tail.y -> down

        if math.abs(nextHead.x - tail.x) >
      */
      if (math.abs(nextHead.x - tail.x) > math.abs(nextHead.y - tail.y)) {
        if (nextHead.x < tail.x) {
          Position(nextHead.x + 1, nextHead.y)
        } else {
          Position(nextHead.x - 1, nextHead.y)
        }
      } else {
        if (nextHead.y > tail.y) {
          Position(nextHead.x, nextHead.y - 1)
        } else {
          Position(nextHead.x, nextHead.y + 1)
        }
      }
      // move.direction match
      //   case Direction.Left => Position(nextHead.x + 1, nextHead.y)
      //   case Direction.Right => Position(nextHead.x - 1, nextHead.y)
      //   case Direction.Up => Position(nextHead.x, nextHead.y - 1)
      //   case Direction.Down => Position(nextHead.x, nextHead.y + 1)
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
          y <- (currentTail.y to nextTail.y by (if (currentTail.y > nextTail.y) -1 else 1))
        } yield Position(nextTail.x, y)).toList
      } else if (currentTail.y == nextTail.y) {
        (for {
          x <- (currentTail.x to nextTail.x by (if (currentTail.x > nextTail.x) -1 else 1))
        } yield Position(x, nextTail.y)).toList
      } else {
        // we have to take a diagonal
        // take the one which has the closest x and y
        val diag = diagonals(currentTail)
          .minBy(p => math.abs(nextTail.x - p.x) + math.abs(nextTail.y - p.y))
        List(diag).appendedAll(tailPath(diag, nextTail))
      }
    }

  def applyMove(s: State, move: Move): State = {
    val nextHeadPos = nextHeadPosition(s.headPosition, move)
    val nextTailPos = nextTailPosition(nextHeadPos, s.tailPosition)
    val tp = tailPath(s.tailPosition, nextTailPos)
    // println("+++")
    // println(("head", s.headPosition))
    // println(("tail", s.tailPosition))
    // println(("move", move))
    // println(("next head", nextHeadPos))
    // println(("next tail", nextTailPos))
    // println(("tail path", tp))
    // println("+++\n")
    State(
      nextHeadPos,
      nextTailPos,
      s.previousTailPositions.appendedAll(tailPath(s.tailPosition, nextTailPos))
    )
  }

  case class TailState(position: Position, previousPositions: List[Position])

  case class MultiTailState(
    headPosition: Position,
    tailPositionsAndPaths: List[TailState]
  )

  def initialMultiTailStates: MultiTailState =
    MultiTailState(Position(0,0), List.fill(9)(TailState(Position(0,0), List())))

  def applyMultiTailMove(s: MultiTailState, move: Move): MultiTailState = {
    val nextHeadPos = nextHeadPosition(s.headPosition, move)
    val (ns, newTailStates) = s.tailPositionsAndPaths.foldLeft((nextHeadPos, List.empty[TailState]))((acc, ts) => {
      val (nextHead, tailStates) = acc
      val nextTailPos = nextTailPosition(nextHead, ts.position)
      val newPrevPositions = ts.previousPositions.appendedAll(tailPath(ts.position, nextTailPos))
      (nextTailPos, tailStates.appendedAll(List(TailState(nextTailPos, newPrevPositions))))
    })
    MultiTailState(nextHeadPos, newTailStates)
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

  override def part1(input: Stream[IO, String]): IO[String] =
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

  override def part2(input: Stream[IO, String]): IO[String] =
   input
      .through(text.lines)
      .filter(_ != "")
      .map(parseMove)
      .fold(initialMultiTailStates)(applyMultiTailMove)
      .compile
      .toList
      .map(_.headOption.map(s => {
        s.tailPositionsAndPaths.reverse.head.previousPositions.toSet.size.toString
      }).getOrElse("Failed"))

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
    println(endState.previousTailPositions.toSet.size)
    println(renderPositions(endState.previousTailPositions.toSet, 4, 5))

    val lines2 = List(
      "R 5",
      "U 8",
      "L 8",
      "D 3",
      "R 17",
      "D 10",
      "L 25",
      "U 20",
    )
    val endState2 = lines2.map(parseMove).foldLeft(initialMultiTailStates)(applyMultiTailMove)
    println(endState2.tailPositionsAndPaths.reverse.head.previousPositions.toSet.size)
    // tail 9 visits 36 positions
  }

  import scala.io.Source

  def runOnInput: Unit = {
    val data = Source.fromFile("notes/day9/input.txt").getLines.toList
    val endState = data.map(parseMove).foldLeft(initialStates)(applyMove)
    println(endState.previousTailPositions.toSet.size)

    val endState2 = data.map(parseMove).foldLeft(initialMultiTailStates)(applyMultiTailMove)
    println(endState2.tailPositionsAndPaths.reverse.head.previousPositions.toSet.size)
  }
}
