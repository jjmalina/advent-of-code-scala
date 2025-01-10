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

  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .compile
      .toList
      .map(ls => {
        val (a, s) = parseAlmanac(ls)
        println(a.mappings)
        val rangeTuples = parseRangeTuples(s)
        println(a.rangedSeedLocations(rangeTuples).filter(r => r._1 == 0 && r._2 > 31394121))
        a.rangedSeedLocations(rangeTuples).map(_._1).min.toString
      })

  case class SourceDestinationRange(destinationStart: Long, sourceStart: Long, length: Long) {
    def existsInSourceRange(i: Long): Boolean = sourceStart <= i && i < sourceStart + length
    def findDestination(i: Long): Option[Long] = {
      if (existsInSourceRange(i)) {
        Some(destinationStart + (i - sourceStart))
      } else {
        None
      }
    }
    def hasSourceOverlap(start: Long, amount: Long): Boolean =
      ((start <= sourceStart && start + amount >= sourceStart) ||
      (sourceStart <= start && start <= sourceStart + length))

    def findDestinationRanges(start: Long, amount: Long): Option[List[(Long, Long)]] = {
      // if (sourceStart == 0) {
      //   println((start, amount, this))
      // }
      if (hasSourceOverlap(start, amount)) {
        val end = start + amount
        // if some portion of the range is before the source start it gets mapped to itself
        val beforeRange = if (start < sourceStart)
          List((start, sourceStart - start)) else List()

        // if some portion of the range is after the source end it gets mapped to itself
        val afterRange = if (end > sourceStart + length)
          List((sourceStart + length, end - (sourceStart + length))) else List()

        // now we have to translate to the destination
        val sourceEnd = sourceStart + length

        val newStart = if (start > sourceStart) then start else sourceStart
        val newEnd = if (end < sourceEnd) then end else sourceEnd

        val newAmount = if (newEnd - newStart > amount) amount else newEnd - newStart

        val sourceOffset = sourceStart - start
        val destinationOffset = sourceStart - destinationStart
        val translatedStart = newStart - destinationOffset
        val r = (beforeRange ++ List((translatedStart, newAmount)) ++ afterRange).filter(_._2 > 0)
        // if (r.exists(_._1 == 0)) {
        //   println((start, amount, this, r))
        // }
        if (sourceStart == 0) {
          println((start, amount, this, r))
        }
        Some(r)
      } else {
        None
      }
    }
  }
  case class SourceDestination(name: String, ranges: List[SourceDestinationRange]) {
    def findDestination(i: Long): Long =
      ranges.flatMap(_.findDestination(i)).headOption.getOrElse(i)
    def findRangedDestination(start: Long, amount: Long): List[(Long, Long)] = {
      val locationRanges = ranges.flatMap(r => r.findDestinationRanges(start, amount)).flatten

      if (locationRanges.length == 0) List((start, amount)) else locationRanges
    }
  }
  case class Almanac(mappings: List[SourceDestination] = List()) {
    def seedLocation(seed: Long): Long = {
      mappings.foldLeft(seed)((loc, mapping) => mapping.findDestination(loc))
    }

    def minimumSeedLocation(seeds: List[Long]): Long = seeds.map(seedLocation(_)).min

    def rangedSeedLocations(seedRanges: List[(Long, Long)]): List[(Long, Long)] =
      seedRanges.map(
        sr => {
          mappings.foldLeft(List(sr))(
            (currentRanges, mapping) => {
              // println(mapping.name)
              currentRanges.flatMap(
              r => mapping.findRangedDestination.tupled(r))
            })
        }
      ).flatten

    def minLocationForSeedRanges(seedRanges: List[(Long, Long)]): Long =
      rangedSeedLocations(seedRanges).map(_._1).min

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
    val locationRanges =  alm.rangedSeedLocations(rangeTuples)
    println(locationRanges)
    println(locationRanges.map(_._1).min)
  }
}
