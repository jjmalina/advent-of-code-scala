package com.jjmalina.aoc.y2023

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day04 extends AOCApp(2023, 4) {
  def part1(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_ != "\n")
      .map(parseCard)
      .map(_.map(c => c.points).getOrElse(0))
      .fold(0)(_ + _)
      .compile
      .toList
      .map(_.head.toString)

  def part2(input: Stream[IO, String]): IO[String] = input
      .through(text.lines)
      .filter(_ != "\n")
      .map(parseCard)
      .compile
      .toList
      .map(parsedCards => accumulateWinningCards(parsedCards.flatten).values.toList.sumAll.toString)

  case class Card(id: Int, winningNumbers: List[Int], numbers: List[Int]) {
    def numbersWon: Set[Int] = winningNumbers.toSet.intersect(numbers.toSet)
    def wins: Int = numbersWon.size
    def points: Int = {
      if wins > 1 then Math.pow(2, wins - 1).toInt else wins
    }
    def wonCardIds: List[Int] =
      if (wins > 0) then (id + 1 to id + wins).toList else List.empty[Int]
  }

  def parseNumbers(s: String): List[Int] = s.split(" ").toList.flatMap(_.toIntOption)

  def parseCard(line: String): Option[Card] = {
    line match {
      case s"Card   $id: $winning | $numbers" =>
        Some(Card(id.toInt, parseNumbers(winning), parseNumbers(numbers)))
      case s"Card  $id: $winning | $numbers" =>
        Some(Card(id.toInt, parseNumbers(winning), parseNumbers(numbers)))
      case s"Card $id: $winning | $numbers" =>
        Some(Card(id.toInt, parseNumbers(winning), parseNumbers(numbers)))
      case _ => None
    }
  }

  def accumulateWinningCards(cards: List[Card]): Map[Int, Int] = {
    cards.foldLeft(cards.map(card => (card.id, 1)).toMap)((accumulatedCardIds, card) => {
      val numberOfSameCard = accumulatedCardIds(card.id)
      val updatedWonCards = card.wonCardIds.foldRight(Map.empty[Int, Int])((id, m) => {
        m.updated(id, numberOfSameCard)
      })
      Monoid.combineAll(List(accumulatedCardIds, updatedWonCards))
    })
  }

  val sampleInput = List(
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
  )

  def runOnSample1: Unit = {
    sampleInput.flatMap(parseCard(_)).foreach(card => println((card.id, card.numbersWon.size, card.points)))
  }

  def runOnSample2: Unit = {
    val cards = sampleInput.flatMap(parseCard(_)).toList
    println(accumulateWinningCards(cards).values.toList.sumAll)
  }
}
