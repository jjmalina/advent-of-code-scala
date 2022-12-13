package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day10 extends AOCApp(2022, 10) {
  enum Op {
    case AddV(value: Int)
    case Noop
  }

  def parseLine(line: String): Op =
    line match {
      case s"addx $value" => Op.AddV(value.toInt)
      case s"noop" => Op.Noop
    }

  case class State(cycle: Int, value: Int)

  def initialState: State = State(1, 1)

  def updateState(acc: (State, List[State]), op: Op): (State, List[State]) = {
    val (s, lss) = acc
    val newStates = op match
      case Op.AddV(value) => (1 to 3).map(i => State(s.cycle + i, if (i == 3) s.value + value else s.value)).toList
      case Op.Noop => List(s.copy(cycle=s.cycle + 1))
    (newStates.reverse.head, lss.appendedAll(newStates))
  }

  def solve(input: Stream[IO, String]): IO[String] = {
   ???
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    ???

  override def part2(input: Stream[IO, String]): IO[String] =
    ???

  import scala.io.Source

  def runExamples: Unit = {
    val small = List(
      "noop",
      "addx 3",
      "addx -5",
    )
    println(small.map(parseLine).foldLeft((initialState,List(initialState)))(updateState))

    val data = Source.fromFile("notes/day10/example.txt").getLines.toList
    val (current, states) = data.map(parseLine).foldLeft((initialState,List(initialState)))(updateState)
    val values = List(20, 60, 100, 140, 180, 220)
      .map(cycle => states.find(_.cycle == cycle))
    println(values)
    val answer = values
      .sequence
      .map(ls => ls.map(s=> s.cycle * s.value).sum.toString)
      .getOrElse("wtf")
    println(answer)
  }
}
