package com.jjmalina.aoc.y2023

import scala.io.Source

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day05 extends AOCApp(2023, 5) {

  override def part1(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .compile
      .toList
      .map(ls => {
        val (a, s) = parseAlmanac(ls)
        a.minimumSeedLocation(s).toString
      })

  override def part2(input: Stream[IO, String]): IO[String] = input
      .through(text.lines)
      .compile
      .toList
      .map(ls => {
        val (a, s) = parseAlmanac(ls)
        val rangeTuples = parseRangeTuples(s)
        a.minimumRangedSeedLocation(rangeTuples).toString
      })


  case class SourceDestinationRange(destinationStart: Long, sourceStart: Long, length: Long) {
    def hasSourceOverlap(start: Long, amount: Long): Boolean =
      ((start <= sourceStart && start + amount <= sourceStart + length) ||
      (sourceStart <= start && sourceStart + length <= start + amount))
    def existsInSourceRange(i: Long): Boolean = sourceStart <= i && i < sourceStart + length
    def findDestination(i: Long): Option[Long] = {
      if (existsInSourceRange(i)) {
        Some(destinationStart + (i - sourceStart))
      } else {
        None
      }
    }
  }
  case class SourceDestination(name: String, ranges: List[SourceDestinationRange]) {
    def findDestination(i: Long): Long =
      ranges.flatMap(_.findDestination(i)).headOption.getOrElse(i)
  }
  case class Almanac(mappings: List[SourceDestination] = List()) {
    def seedLocation(seed: Long): Long = {
      mappings.foldLeft(seed)((loc, mapping) => mapping.findDestination(loc))
    }

    def minimumSeedLocation(seeds: List[Long]): Long = seeds.map(seedLocation(_)).min

    def minimumRangedSeedLocation(seedRanges: List[(Long, Long)]): Long = {
      seedRanges.map {
        case (from, amount) => {

          (from to from + amount - 1L).foldRight(Long.MaxValue)((seed, currentMin) => {
            val location = seedLocation(seed)
            if (location < currentMin) then location else currentMin
          })
        }
      }.min
    }
  }

  def parseRangeTuples(rangeTuples: List[Long]): List[(Long, Long)] =
    rangeTuples.grouped(2).map(l => (l(0), l(1))).toList

  def parseRange(s: String): Option[SourceDestinationRange] = s match {
    case s"$dest $source $len" => Some(SourceDestinationRange(dest.toLong, source.toLong, len.toLong))
    case _ => None
  }

  def parseAlmanac(ls: List[String]): (Almanac, List[Long]) = {
    val initial = (Almanac(), Option.empty[SourceDestination], List.empty[Long])
    val (newAlmanac, _, seeds) = ls.foldLeft(initial)((acc, line) => {
      val (currentAlm, currentSourceDest, currentSeeds) = acc
      line match {
        case s"seeds: $seeds" =>
          (currentAlm, None, seeds.split(" ").toList.map(_.toLong))
        case "" => {
          currentSourceDest match {
            case Some(sd) => (currentAlm.copy(mappings=currentAlm.mappings.appended(sd)), None, currentSeeds)
            case None => acc
          }
        }
        case s"$name map:" => (currentAlm, Some(SourceDestination(name, List())), currentSeeds)
        case s => {
          currentSourceDest match {
            case Some(sd) => (currentAlm, parseRange(s).map(r => sd.copy(ranges=sd.ranges.appended(r))), currentSeeds)
            case None => acc
          }
        }
      }
    })
    (newAlmanac, seeds)
  }

  val sampleInput = List(
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4",
    "",
  )

  def runOnSample1: Unit = {
    val (alm, seeds) = parseAlmanac(sampleInput)
    println(seeds.map(alm.seedLocation))
    println(alm.minimumSeedLocation(seeds))
  }

  def runOnSample2: Unit = {
    val (alm, seeds) = parseAlmanac(sampleInput)
    val rangeTuples = parseRangeTuples(seeds)
    println(alm.minimumRangedSeedLocation(rangeTuples))
  }
}
