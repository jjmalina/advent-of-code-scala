package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day01 extends AOCApp(2022, 1) {

  def totalCalories(input: Stream[IO, String]): Stream[IO, List[List[Int]]] =
    input
      .through(text.lines)
      .fold(List.empty[List[Int]])((accum, line) => {
        line match {
          case "" => {
            accum match {
              case current :: rest => {
                rest.appended(current).prepended(List.empty[Int])
              }
              case Nil => List(List.empty[Int])
            }
          }
          case s => {
            accum match {
              case current :: rest => rest.prepended(current :+ s.toInt)
              case Nil => List(List.empty[Int])
            }
          }
        }
      })

  def solve(input: Stream[IO, String]): IO[String] = {
    totalCalories(input)
      .compile
      .toList
      .map(ll => {
        ll.head.map(_.sum).sortBy(-_).take(1).toString
      })
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    totalCalories(input)
      .compile
      .toList
      .map(ll => {
        ll.head.map(_.sum).sortBy(-_).take(1).head.toString
      })

  override def part2(input: Stream[IO, String]): IO[String] =
    totalCalories(input)
      .compile
      .toList
      .map(ll => {
        ll.head.map(_.sum).sortBy(-_).take(3).sum.toString
      })

}
