package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day10 extends AOCApp(2022, 10) {
  enum Op {
    case AddX(value: Int)
    case Noop
  }

  def parseLine(line: String): Op =
    line match {
      case s"addx $value" => Op.AddX(value.toInt)
      case s"noop" => Op.Noop
    }

  case class State(cycle: Int, value: Int)

  def initialState: State = State(0, 1)

  def updateState(acc: (State, List[State]), op: Op): (State, List[State]) = {
    val (s, lss) = acc
    val newStates = op match
      case Op.AddX(value) => (1 to 2).map(
        i => State(s.cycle + i, if (i == 2) s.value + value else s.value)
      ).toList
      case Op.Noop => List(s.copy(cycle=s.cycle + 1))
    (newStates.reverse.head, lss.appendedAll(newStates))
  }

  def drawScreen(states: List[State]): String = {
    val pixels = states.map(
      s => if (List(s.value - 1, s.value, s.value + 1).contains(s.cycle % 40)) "#" else ".")
    val (rows, _) = (1 to 6).foldLeft((List.empty[List[String]], pixels))((acc, _) => {
      val (rows, curr) = acc
      if (curr.length <= 40) {
        (rows.appended(curr), List())
      } else {
        val (newRow, rest) = curr.splitAt(40)
        (rows.appended(newRow), rest)
      }
    })
    rows.map(_.mkString("")).mkString("\n")
  }

  def solvePart1(states: List[State]): String = {
    val values = List(20, 60, 100, 140, 180, 220)
      .map(cycle => states.find(_.cycle == cycle - 1))
    values
      .sequence
      .map(ls => ls.map(s=> (s.cycle + 1) * s.value).sum.toString)
      .getOrElse("Not found")
  }

  def solve(input: Stream[IO, String], f: (List[State]) => String): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .map(parseLine)
      .fold((initialState,List(initialState)))(updateState)
      .compile
      .toList
      .map(_.headOption.map(acc => {
        val (currentState, states) = acc
        f(states)
      }).getOrElse("Not found"))
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, solvePart1)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, drawScreen)

  import scala.io.Source

  def runExamples: Unit = {
    val small = List(
      "noop",
      "addx 3",
      "addx -5",
    )
    println(small.map(parseLine).foldLeft((initialState,List(initialState)))(updateState))

    val data = Source.fromFile("notes/day10/example.txt").getLines.toList
    val (current, states) = data
      .map(parseLine)
      .foldLeft((initialState,List(initialState)))(updateState)
    println(solvePart1(states))
    println(drawScreen(states))
  }
}
