package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day06 extends AOCApp(2022, 6) {

  def differentSequenceStart(s: String, size: Int): Option[Int] = {
    val r = s.toList.sliding(size, 1).toList.zipWithIndex.find((item) => {
      val (chars, idx) = item
      chars.toSet.size == size
    })
    r.map(_._2 + size)
  }

  def solve(input: Stream[IO, String], size: Int): IO[String] = {
    input
      .through(text.lines)
      .map(differentSequenceStart(_, size))
      .compile
      .toList
      .map(_.head.map(_.toString).getOrElse("Not found!"))
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, 4)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, 14)

  def runExamples: Unit = {
    List(
      "mjqjpqmgbljsphdztnvjfqwrcgsmlb",  // 7
      "bvwbjplbgvbhsrlpgdmjqwftvncz", // 5
      "nppdvjthqldpwncqszvftbrmjlhg",  // 6
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",  // 10
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  // 11
    ).foreach(s => println(differentSequenceStart(s, 4)))
    List(
      "mjqjpqmgbljsphdztnvjfqwrcgsmlb",  // 19
      "bvwbjplbgvbhsrlpgdmjqwftvncz", // 23
      "nppdvjthqldpwncqszvftbrmjlhg",  // 23
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",  // 29
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  // 26
    ).foreach(s => println(differentSequenceStart(s, 14)))
  }
}
