package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day07 extends AOCApp(2022, 7) {
  def goUpDirLevel(p: String): String =
    if (p == "/")
      "/"
    else {
      val r = p.split("/").toList.reverse.tail.reverse.mkString("/")
      if (r == "") "/" else r
    }

  def goDownDirLevel(p: String, d: String): String =
    if (d == "/" || p == "/") s"$p$d" else s"$p/$d"

  def joinFile(p: String, fname: String): String =
    if (p == "/") s"$p$fname" else s"$p/$fname"

  def aggregateCurrentDirFiles(
    dirFile: (String, Map[String, Long]),
    line: String
  ): (String, Map[String, Long]) = {
    val (currentDir, fileMap) = dirFile
    line match {
      case s"$$ cd $rest" => (changeDir(currentDir, line), fileMap)
      case s"$$ ls" => dirFile
      case s"dir $d" => dirFile
      case s"$size $filename" =>
        (currentDir, dirFile._2.updated(joinFile(currentDir, filename), size.toLong))
    }
  }

  def directorySizes(files: List[(String, Long)]): List[(String, Long)] = {
    files.flatMap {
      case (filename, size) => {
        val dirs = filename.split("/").toList
        val (_, dirSizes) = dirs.slice(0, dirs.length - 1).foldLeft(("", List.empty[(String, Long)]))((currentDirAndDirSizes, dir) => {
          val (currentDir, dirSizes) = currentDirAndDirSizes
          val newDir = goDownDirLevel(currentDir, dir)
          (newDir, (newDir, size) :: dirSizes)
        })
        dirSizes
      }
    }
  }

  def changeDir(currentDir: String, line: String): String = {
    line match {
      case s"""$$ cd ..""" => goUpDirLevel(currentDir)
      case s"""$$ cd $dir""" => goDownDirLevel(currentDir, dir)
      case _ => currentDir
    }
  }

  def aggregateDirectorySizes(aggregatedFiles: Map[String, Long]): Map[String, Long] =
    directorySizes(aggregatedFiles.toList).foldRight(Map.empty[String, Long]) {
      case ((key, size), directorySizes) =>
        directorySizes.updated(key, directorySizes.getOrElse(key, 0L) + size)
    }

  def solve(input: Stream[IO, String]): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .fold(("", Map.empty[String, Long]))(aggregateCurrentDirFiles)
      .compile
      .toList
      .map(aggregates => aggregates.map((aggregatedFiles: (String, Map[String, Long])) => {
        val atMost = aggregateDirectorySizes(aggregatedFiles._2).filter(_._2 <= 100_000)
        atMost.values.sum.toString
      }).head)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input)

  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_ != "")
      .fold(("", Map.empty[String, Long]))(aggregateCurrentDirFiles)
      .compile
      .toList
      .map(aggregates => aggregates.map((aggregatedFiles: (String, Map[String, Long])) => {
        val aggregatedDirectories = aggregateDirectorySizes(aggregatedFiles._2)
        val totalSpace = 70000000
        val neededSpace = 30000000
        val usedSpace = aggregatedDirectories("/")
        val freeSpace = totalSpace - usedSpace
        val neededToFree = neededSpace - freeSpace
        aggregatedDirectories
          .toList
          .sortBy(_._2)
          .find(_._2 >= neededToFree)
          .map(_._2.toString)
          .getOrElse("Not found")
      }).head)

  def runExamples: Unit = {
    val r = List(
      "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k",
    ).foldLeft(("", Map.empty[String, Long]))(aggregateCurrentDirFiles)
    val part1 = aggregateDirectorySizes((r._2)).filter(_._2 <= 100_000)
    println(part1)
  }
}
