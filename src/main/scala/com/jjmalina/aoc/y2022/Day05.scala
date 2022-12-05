package com.jjmalina.aoc.y2022

import scala.collection.immutable.Queue
import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day05 extends AOCApp(2022, 5) {
  def dequeueN[A](queue: Queue[A], n: Int): (List[A], Queue[A]) =
    (0 until n).foldRight((List.empty[A], queue))((_, q) => {
      val (items, qq) =  q
      val (item, newQueue) = qq.dequeue
      (item :: items, newQueue)
    })

  type Stacks = Map[Int, Queue[Char]]

  case class Move(crates: Int, from: Int, to: Int)

  def parseMove(s: String): Move = {
    val fragments = s.split(" from ")
    val crates = fragments(0).replace("move ", "").toInt
    val fromToFragments = fragments(1).split(" to ")
    Move(crates, fromToFragments(0).toInt, fromToFragments(1).toInt)
  }

  def applyMove(stacks: Stacks, move: Move): Stacks = {
    val (removed, fromQueue) = dequeueN(stacks(move.from), move.crates)
    val toQueue = stacks(move.to).prependedAll(removed)
    stacks.updated(move.from, fromQueue).updated(move.to, toQueue)
  }

  def applyMove9001(stacks: Stacks, move: Move): Stacks = {
    val (removed, fromQueue) = dequeueN(stacks(move.from), move.crates)
    val toQueue = stacks(move.to).prependedAll(removed.reverse)
    stacks.updated(move.from, fromQueue).updated(move.to, toQueue)
  }

  def parseIntoStacks(ls: List[String]): Stacks = {
    ls.foldRight(Map.empty[Int, Queue[Char]])((stackString, stacks) => {
      val chars = stackString.reverse.toList
      val key = chars.head.asDigit
      stacks.updated(key, chars.drop(1).foldRight(Queue.empty[Char])((c, q) => q.enqueue(c)))
    })
  }

  def initialStackState(input: Stream[IO, String]): Stream[IO, String] =
    input.through(text.lines).takeWhile(line => line != "")

  def stackMoves(input: Stream[IO, String]): Stream[IO, Move] =
    input
      .through(text.lines)
      .dropWhile(line => line != "")
      .filter(line => line != "")
      .map(parseMove(_))

  def solve(
    input: Stream[IO, String],
    updateState: (Stacks, Move) => Stacks
  ): IO[String] = {
    val moves = stackMoves(input)
    initialStackState(input).compile.toList.flatMap(ls => {
      val s = ls.transpose.filter(_.last.isDigit).map(_.mkString.replace(" ", ""))
      val stacks = parseIntoStacks(s)
      moves.fold(stacks)((s, move) => updateState(s, move))
        .compile.toList.map(_.head.toList.sortBy(_._1).map(_._2.head).mkString(""))
    })

  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, applyMove)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, applyMove9001)

  def runExample: Unit = {
    val stacks = Map(
      1 -> Queue('N', 'Z'),
      2 -> Queue('D', 'C', 'M'),
      3 -> Queue('P'),
    )
    val moves = List(
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2",
    )
    val result1 = moves
      .map(parseMove(_))
      .foldLeft(stacks)((stacks, move) => applyMove(stacks, move))
      .toList.sortBy(_._1).map(_._2.head).mkString("")
    println(result1)

    val result2 = moves
      .map(parseMove(_))
      .foldLeft(stacks)((stacks, move) => applyMove9001(stacks, move))
      .toList.sortBy(_._1).map(_._2.head).mkString("")
    println(result2)
    ()
  }

}
