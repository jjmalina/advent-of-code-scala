package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import scala.collection.mutable.{Queue, Stack}
import cats.instances.short
// import _root_.io.circe._, _root_.io.circe.parser._

object Day13 extends AOCApp(2022, 13) {
  enum Packet:
    case Single(num: Int)
    case Multi(packets: List[Packet])

  def enqueueLine(s: String): Queue[String] = {
    val q = new Queue[String]()
    var currentS = ""
    s.foreach(c => {
      c match {
        case '[' => {
          q.enqueue(c.toString)
        }
        case ']' => {
          if (currentS.length > 0) {
            q.enqueue(currentS)
            currentS = ""
          }
          q.enqueue(c.toString)
        }
        case ',' => {
          if (currentS.length > 0)
            q.enqueue(currentS)
          currentS = ""
          ()
        }
        case _ => currentS += c.toString
      }
    })
    q
  }

  def packetsInOrder(leftInput: String, rightInput: String): Boolean = {
    println(s"\n$leftInput vs $rightInput")
    val left = enqueueLine(leftInput)
    val right = enqueueLine(rightInput)
    val leftArrayContext = new Stack[String]();
    val rightArrayContext = new Stack[String]();
    if (left.isEmpty && right.isEmpty) {
      true
    } else if (left.isEmpty) {
      true
    } else {
      var leftEmpty = false
      var rightEmpty = false
      var rv = true
      var shortCircuit = false

      while
        (left.length > 0 && right.length > 0 && !shortCircuit)
      do {
        val l = left.dequeue()
        val r = right.dequeue()
        println((l, r))
        (l, r) match
          case ("[", "[") => {
            leftArrayContext.push("[")
            rightArrayContext.push("[")
          }
          case ("]", "]") => ()
          case ("[", "]") => {
            shortCircuit = true
            rv = false
          }
          case ("[", number) => {
            right.prepend(number)
          }
          case (number, "[") => {
            left.prepend(number)
          }
          case ("]", number) => {
            shortCircuit = true
            rv = true
          }
          case (number, "]") => {
            shortCircuit = true
            rv = false
          }
          case (number, number2) => {
            val leftNum = number.toInt
            val rightNum = number2.toInt
            if (leftNum < rightNum) {
              shortCircuit = true
              rv = true
            } else if (leftNum != rightNum){
              shortCircuit = true
              rv = false
            }
          }
      }
      rv
    }
  }

  def solve(input: Stream[IO, String]): IO[String] = {
    input
      .through(text.lines)
      .fold(((Option.empty[String], Option.empty[String]), List.empty[Option[(String, String)]]))((acc, line) => {
        val (current, pairs) = acc
        line match {
          case "" => (
            (None, None),
            pairs.appended(current._1.flatMap(c => current._2.map((c, _))))
          )
          case line => if (current._1.isDefined)
            ((current._1, Some(line)), pairs) else ((Some(line), None), pairs)
        }
      })
      .compile
      .toList
      .map(_.headOption.flatMap(acc => {
        val (_, listOfPairs) = acc
        listOfPairs
          .sequence
          .map(pairs => {
            val results = pairs.map(
              pair => {
                val inOrder = packetsInOrder(pair._1, pair._2)
                println(pair._1)
                println(pair._2)
                println(inOrder)
                println("\n")
                inOrder
              }
            ).zipWithIndex.map(row => (row._1, row._2 + 1)).filter(_._1)
            // results.foreach(println)
            results.map(_._2).sum.toString
          })
      }).getOrElse("Not found"))
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input)

  def runExamples: Unit = {
    val examples = List(
      ("[1,1,3,1,1]", "[1,1,5,1,1]", true),
      ("[[1],[2,3,4]]", "[[1],4]", true),
      ("[9]", "[[8,7,6]]", false),
      ("[[4,4],4,4]", "[[4,4],4,4,4]", true),
      ("[7,7,7,7]", "[7,7,7]", false),
      ("[]", "[3]", true),
      ("[[[]]]", "[[]]", false),
      ("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]", false),
    )
    val result = examples.map {
      case (l, r, expected) => {
        val rv = packetsInOrder(l, r)
        try {
          assert(rv == expected)
        } catch {
          case _: Throwable =>
            throw new Exception(s"failed on $l vs $r, expected $expected, got $rv")
        }
      }
    }
    println(result)
  }

}
