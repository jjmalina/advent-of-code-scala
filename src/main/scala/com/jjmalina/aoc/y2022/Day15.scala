package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day15 extends AOCApp(2022, 15) {
  /*
    new idea
      for each sensor, find its y intercept
      then generate a set of lines
        top left top right
        bottom left bottom ight

        top left and bottom right have a slope of 1
        top right and bottom left have a slope of -1

        increment decrement the y intercepts of all lines until we find the nearest sensor on one of them
          to solve whether the beacon exists on a line
            beacon y = beacon x + <intercept>

      so now every sensor has its containing lines
      we need to generate a set of points at a given Y for all sensors
        for each line intercept
          Y = x + <line-intercept> -> Y - <line-intercept> = topLeftX
          Y = x + <line-intercept> -> Y - <line-intercept> = topRightX
          Y = x + <line-intercept> -> Y - <line-intercept> = bottomLeftX
          Y = x + <line-intercept> -> Y - <line-intercept> = bottomRightX

      now generate points on Y that are inside the perimeter
        if Y >= sensor.y then we calculate topLeftX and topRight X
          we generate points between topLeftX and topRightX but only if topRightX > topLeftX
        else
          we calculate bottomLeftX and bottomRightX and generate x between but only if bottomRightX > bottomLeftX

  */
  case class Point(x: Int, y: Int)
  case class SensorWithBeacon(sensor: Point, beacon: Point) {
    def immediateBottom = Point(sensor.x, sensor.y + 1)
    def scannedPerimeterPoints(startingBottomPoint: Point): Set[Point] = {
      /*
       draw the bottom left side -x -y until you reach same y as sensor
       draw the top left side +x -y until you reach the same x as sensor
       draw the top right side +x +y until you reach the same y as sensor
       draw the bottom right side -x +y until you reach the same x as sensor
      */
      val bottomLeftSidePoints = List.unfold(startingBottomPoint)(currentPoint => {
        if (currentPoint.y == sensor.y) {
          None
        } else {
          val nextPoint = Point(currentPoint.x - 1, currentPoint.y - 1)
          Some((nextPoint, nextPoint))
        }
      })
      val leftPoint = bottomLeftSidePoints.minBy(_.x)
      val topLeftSidePoints = List.unfold(leftPoint)(currentPoint => {
        if (currentPoint.x == sensor.x) {
          None
        } else {
          val nextPoint = Point(currentPoint.x + 1, currentPoint.y - 1)
          Some((nextPoint, nextPoint))
        }
      })
      val topPoint = topLeftSidePoints.minBy(_.y)
      val topRightSidePoints = List.unfold(topPoint)(currentPoint => {
        if (currentPoint.y == sensor.y) {
          None
        } else {
          val nextPoint = Point(currentPoint.x + 1, currentPoint.y + 1)
          Some((nextPoint, nextPoint))
        }
      })
      val rightPoint = topRightSidePoints.maxBy(_.x)
      val bottomRightSidePoints = List.unfold(rightPoint)(currentPoint => {
        if (currentPoint == startingBottomPoint) {
          None
        } else {
          val nextPoint = Point(currentPoint.x - 1, currentPoint.y + 1)
          Some((nextPoint, nextPoint))
        }
      })
      bottomLeftSidePoints.toSet ++
        topLeftSidePoints.toSet ++
        topRightSidePoints.toSet ++
        bottomRightSidePoints
    }

    def scannedPointsUntilBeacon: Set[Point] = {
      List.unfold((immediateBottom, false))(((currentBottom: Point, beaconReached: Boolean) => {
        if (beaconReached) {
          None
        } else {
          val newPerimeter = scannedPerimeterPoints(currentBottom)
          val newBottom = Point(sensor.x, newPerimeter.maxBy(_.y).y + 1)
          Some((newPerimeter, (newBottom, newPerimeter.contains(beacon))))
        }
      }).tupled).reduce(_ ++ _)
    }
  }

  object SensorWithBeacon {
    def parse(s: String): Option[SensorWithBeacon] = {
      s match {
        case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" => {
          for {
            sensor <- (sensorX.toIntOption, sensorY.toIntOption).bisequence.map(Point.apply.tupled)
            beacon <- (beaconX.toIntOption, beaconY.toIntOption).bisequence.map(Point.apply.tupled)
          } yield SensorWithBeacon(sensor, beacon)
        }
        case _ => None
      }
    }
  }

  def solvePart1(sensorsWithBeacons: List[SensorWithBeacon], y: Int): Int = {
    val total = sensorsWithBeacons.length
    val scannedPoints = sensorsWithBeacons
      .map(_.scannedPointsUntilBeacon.filter(_.y == y))
      .reduce(_ ++ _)

    val beacons = sensorsWithBeacons.map(_.beacon).toSet
    scannedPoints.filter(p => p.y == y && !beacons.contains(p)).size
  }

  def solve(input: Stream[IO, String], y: Int): IO[String] = {
    input
      .through(text.lines)
      .filter(_ != "")
      .map(SensorWithBeacon.parse)
      .compile
      .toList
      .map(parsedLines =>
        parsedLines
          .sequence
          .map(solvePart1(_, y).toString)
          .getOrElse("Failed")
      )
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, 2_000_000)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, 2_000_000)

  def runExamples: Unit = {
    val input = List(
      "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
      "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
      "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
      "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
      "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
      "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
      "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
      "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
      "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
      "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
      "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
      "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
      "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
      "Sensor at x=20, y=1: closest beacon is at x=15, y=3",
    )
    val pt1 = input.map(SensorWithBeacon.parse).sequence.map(solvePart1(_, 10)).getOrElse("Not found")
    println(pt1)
  }

}
