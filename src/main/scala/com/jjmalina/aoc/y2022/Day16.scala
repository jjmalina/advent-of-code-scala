package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day16 extends AOCApp(2022, 16) {
  case class Valve(
    name: String,
    flowRate: Int,
    tunnelsToValves: List[String],
    opened: Boolean = false,
  )

  object Valve {
    def parseTunnels(s: String): Option[List[String]] = {
      s match {
        case s"tunnels lead to valves $tunnels" => Some(tunnels.split(", ").toList)
        case s"tunnel leads to valve $tunnel" => Some(List(tunnel))
        case _ => None
      }
    }
    def parse(s: String): Option[Valve] = {
      s match {
        case s"Valve $name has flow rate=$rate; $tunnelsInput" => {
          for {
            flowRate <- rate.toIntOption
            tunnels <- parseTunnels(tunnelsInput)
          } yield Valve(name, flowRate, tunnels)
        }
        case _ => None
      }
    }
  }

  case class State(
    currentValve: Valve,
    minutesRemaining: Int,
    valves: Map[String, Valve],
    pressureReleased: Int,
  ) {
    def canProceed: Boolean = minutesRemaining > 1
    def currentPressure: Int = valves.values.filter(_.opened).map(_.flowRate).sum
    def moveToValve(v: String): State = {
      this.copy(
        currentValve=valves(v),
        minutesRemaining=minutesRemaining - 1,
        pressureReleased=pressureReleased + currentPressure
      )
    }
    def openCurrentValve: State = openValve(currentValve)
    def openValve(v: Valve): State = {
      val updatedValve = v.copy(opened=true)
      val newValves = valves.updated(updatedValve.name, updatedValve)
      this.copy(
        currentValve=updatedValve,
        valves = newValves,
        minutesRemaining=minutesRemaining - 1,
        pressureReleased=pressureReleased + currentPressure
      )
    }
  }

  def possibleEndStates(s: State): List[State] = {
    if (s.minutesRemaining == 0) {
      List(s)
    } else {
      // can either open current valve, move to another valve, or both
      if (s.currentValve.opened && s.canProceed) {
        s.valves.keys.toList.flatMap(
          name => possibleEndStates(s.moveToValve(name))
        )
      } else if (s.canProceed) {
        val currentValveOpened = s.openCurrentValve
        if (currentValveOpened.canProceed) {
          currentValveOpened.valves.keys.toList.flatMap(
            name => possibleEndStates(currentValveOpened.moveToValve(name))
          )
        } else {
          List(currentValveOpened)
        }
      } else {
        List(s)
      }
    }
  }

  def solvePart1(valves: List[Valve]): Int = {
    val initialValve = valves.find(_.name == "AA").get
    val initialState = State(initialValve, 30, valves.map(v => (v.name, v)).toMap, 0)
    val endState = possibleEndStates(initialState).maxBy(_.pressureReleased)
    endState.pressureReleased
  }

  def solve(
    input: Stream[IO, String],
  ): IO[String] = {
    ???
  }

  override def part1(input: Stream[IO, String]): IO[String] =
   ???

  override def part2(input: Stream[IO, String]): IO[String] =
    ???

  def runExamples: Unit = {
    val input = List(
      "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
      "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
      "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
      "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
      "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
      "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
      "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
      "Valve HH has flow rate=22; tunnel leads to valve GG",
      "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
      "Valve JJ has flow rate=21; tunnel leads to valve II",
    )

    val valves = input.map(Valve.parse).sequence
    println(valves)
    println(valves.map(solvePart1(_)).getOrElse("Not found"))
  }
}
