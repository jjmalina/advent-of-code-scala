package com.jjmalina.aoc.y2022

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day15 extends AOCApp(2022, 15) {
  case class Point(x: Int, y: Int) {
    def positiveSlopeIntercept: Int = y - x  // y = x + b -> y - x = b
    def negativeSlopeIntercept: Int = y + x // y = -x + b -> y + x
    def tuningFrequency: Long = x.toLong * 4_000_000L + y.toLong
  }
  case class Line(slope: Int, intercept: Int) {
    def hasPoint(point: Point): Boolean = point.y == point.x * slope + intercept
    def solveY(x: Int): Point = Point(x, slope * x + intercept)
    def solveX(y: Int): Point = Point((y - intercept) * (if (slope > 0) 1 else -1), y)
  }
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

    def beaconBoundingLines: (Line, Line, Line, Line) = {
      /*
        based on the quadrant from the sensor at the middle we can figure out
        what line the beacon sits on. we can mirror that line over the x axis
        then the lines with the opposite slopes can be derived from the top y
        and bottom y points
      */
      val inTopRightQuadrant = beacon.x > sensor.x && beacon.y > sensor.y
      val inBottomRightQuadrant = beacon.x > sensor.x && beacon.y < sensor.y
      val inTopLeftQuadrant = beacon.x <= sensor.x && beacon.y >= sensor.y
      // val inBottomLeftQuadrant = beacon.x <= sensor.x && beacon.y <= sensor.y
      if (inTopLeftQuadrant) {
        val topLeftLine = Line(1, beacon.positiveSlopeIntercept)
        val diff = beacon.positiveSlopeIntercept - sensor.positiveSlopeIntercept
        val bottomRightLine = Line(1, sensor.positiveSlopeIntercept - diff)
        // find y where x = sensor.x, then find negative intercept of that point
        val topYPoint = topLeftLine.solveY(sensor.x)
        val topRightLine = Line(-1, topYPoint.negativeSlopeIntercept)
        val bottomYPoint = bottomRightLine.solveY(sensor.x)
        val bottomLeftLine = Line(-1, bottomYPoint.negativeSlopeIntercept)
        (topLeftLine, topRightLine, bottomRightLine, bottomLeftLine)
      } else if (inTopRightQuadrant) {
        val topRightLine = Line(-1, beacon.negativeSlopeIntercept)
        val diff = beacon.negativeSlopeIntercept - sensor.negativeSlopeIntercept
        val bottomLeftLine = Line(-1, sensor.negativeSlopeIntercept - diff)
        val topYPoint = topRightLine.solveY(sensor.x)
        val topLeftLine = Line(1, topYPoint.positiveSlopeIntercept)
        val bottomYPoint = bottomLeftLine.solveY(sensor.x)
        val bottomRightLine = Line(1, bottomYPoint.positiveSlopeIntercept)
        (topLeftLine, topRightLine, bottomRightLine, bottomLeftLine)
      } else if (inBottomRightQuadrant) {
        val bottomRightLine = Line(1, beacon.positiveSlopeIntercept)
        val diff = sensor.positiveSlopeIntercept - beacon.positiveSlopeIntercept
        val topLeftLine = Line(1, sensor.positiveSlopeIntercept + diff)
        val topYPoint = topLeftLine.solveY(sensor.x)
        val topRightLine = Line(-1, topYPoint.negativeSlopeIntercept)
        val bottomYPoint = bottomRightLine.solveY(sensor.x)
        val bottomLeftLine = Line(-1, bottomYPoint.negativeSlopeIntercept)
        (topLeftLine, topRightLine, bottomRightLine, bottomLeftLine)
      } else {
        val bottomLeftLine = Line(-1, beacon.negativeSlopeIntercept)
        val diff = sensor.negativeSlopeIntercept - beacon.negativeSlopeIntercept
        val topRightLine = Line(-1, sensor.negativeSlopeIntercept + diff)
        val topYPoint = topRightLine.solveY(sensor.x)
        val topLeftLine = Line(1, topYPoint.positiveSlopeIntercept)
        val bottomYPoint = bottomLeftLine.solveY(sensor.x)
        val bottomRightLine = Line(1, bottomYPoint.positiveSlopeIntercept)
        (topLeftLine, topRightLine, bottomRightLine, bottomLeftLine)
      }
    }

    def extremityPoints(
      topLeft: Line, topRight: Line, bottomRight: Line, bottomLeft: Line
    ): (Point, Point, Point, Point) = {
      (
        topLeft.solveY(sensor.x),
        topRight.solveX(sensor.y),
        bottomRight.solveY(sensor.x),
        bottomLeft.solveX(sensor.y),
      )
    }

    def scannedPointsInsidePerimeterOnY(y: Int): Set[Point] = {
      scannedXRangeOnY(y) match
        case None => Set.empty[Point]
        case Some(value) => value.map(Point(_, y)).toSet
    }

    def scannedXRangeOnY(y: Int): Option[Range.Inclusive] = {
      val (topLeft, topRight, bottomRight, bottomLeft) = beaconBoundingLines
      val (top, right, bottom, left) = extremityPoints(
        topLeft, topRight, bottomRight, bottomLeft
      )
      if (y > top.y || y < bottom.y) {
        None
      } else {
        if (y > sensor.y) {
          // solve for points using topLeft and topRight lines
          val topLeftPoint = topLeft.solveX(y)
          val topRightPoint = topRight.solveX(y)
          Some((topLeftPoint.x to topRightPoint.x))
        } else if (y < sensor.y) {
          val bottomLeftPoint = bottomLeft.solveX(y)
          val bottomRightPoint = bottomRight.solveX(y)
          Some((bottomLeftPoint.x to bottomRightPoint.x))
        } else {
          Some((left.x to right.x))
        }
      }
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

  def solvePart1UsingAlgebra(sensorsWithBeacons: List[SensorWithBeacon], y: Int): Int = {
    val sensors = sensorsWithBeacons.map(_.sensor).toSet
    val beacons = sensorsWithBeacons.map(_.beacon).toSet
    sensorsWithBeacons
      .map(
        _.scannedPointsInsidePerimeterOnY(y).filter(p => !sensors.contains(p) && !beacons.contains(p))
      )
      .reduce(_ ++ _)
      .size
  }

  def solvePart2BruteForce(
    sensorsWithBeacons: List[SensorWithBeacon],
    minX: Int, maxX: Int, minY: Int, maxY: Int
  ): Set[Point] = {
    val sensors = sensorsWithBeacons.map(_.sensor).toSet
    val beacons = sensorsWithBeacons.map(_.beacon).toSet
    val initialPossiblePoints = (for {
      x <- (minX to maxX)
      y <- (minY to maxY)
    } yield Point(x, y)).toSet
    sensorsWithBeacons
      .foldRight(initialPossiblePoints)((sensorWithBeacon, possiblePoints) => {
        possiblePoints - sensorWithBeacon.sensor - sensorWithBeacon.beacon -- {
          val yRange = (minY to maxY).toList
          val z = yRange.map(y => sensorWithBeacon.scannedPointsInsidePerimeterOnY(y))
          z.reduce(_.toSet ++ _)
        }
      })
  }

  def subtractRange(left: Range.Inclusive, right: Range.Inclusive): Option[List[Range.Inclusive]] = {
    if (left.contains(right.start) || left.contains(right.end)) {
      if (left.start < right.start && right.end < left.end) {
        Some(List(
          Range.inclusive(left.start, right.start - 1),
          Range.inclusive(right.end + 1, left.end)
        ))
      } else if (right.start <= left.start) {
        Some(List(Range.inclusive(right.end + 1, left.end)))
      } else  {
        Some(List(Range.inclusive(left.start, right.start - 1)))
      }
    } else if (right.start <= left.start && right.end >= left.end) {
      None
    }
    else {
      Some(List(left))
    }
  }

  def solvePart2(
    sensorsWithBeacons: List[SensorWithBeacon],
    minX: Int, maxX: Int, minY: Int, maxY: Int
  ): Set[Point] = {
    val sensors = sensorsWithBeacons.map(_.sensor).toSet
    val beacons = sensorsWithBeacons.map(_.beacon).toSet
    val initialPossibleX = (minX to maxX)
    (minY to maxY).map(y => {
      val scannedXRanges = sensorsWithBeacons.flatMap(_.scannedXRangeOnY(y))
      val possibleX = scannedXRanges.foldRight(List(initialPossibleX))((scannedRange, possibleRanges) => {
        possibleRanges.flatMap(r => {
          subtractRange(r, scannedRange) match {
            case None => List()
            case Some(r) => r
          }
        })
      })
      if (possibleX.isEmpty) {
        (y, Set())
      } else {
        (y, possibleX.map(_.toSet).reduce(_ ++ _))
      }
    }).flatMap(r => r._2.map(Point(_, r._1)))
      .filter(p => !sensors.contains(p) && !beacons.contains(p)).toSet
  }

  def solvePart1BruteForce(sensorsWithBeacons: List[SensorWithBeacon], y: Int): Int = {
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
          .map(solvePart1UsingAlgebra(_, y).toString)
          .getOrElse("Failed")
      )
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, 2_000_000)

  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_ != "")
      .map(SensorWithBeacon.parse)
      .compile
      .toList
      .map(parsedLines =>
        parsedLines
          .sequence
          .map(solvePart2(_, 0, 4000000, 0, 4000000).map(p => p.tuningFrequency).toList.head.toString)
          .getOrElse("Failed")
      )

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
    val sensorsWithBeacons = input.map(SensorWithBeacon.parse).sequence.get
    val pt1 = solvePart1UsingAlgebra(sensorsWithBeacons, 10)
    println(pt1)

    val sensors = sensorsWithBeacons.map(_.sensor).toSet
    val beacons = sensorsWithBeacons.map(_.beacon).toSet
    val scannedPoints = sensorsWithBeacons.map(_.scannedPointsUntilBeacon).reduce(_ ++ _)
    val maxXsensor = sensors.maxBy(_.x).x
    val maxYSensor = sensors.maxBy(_.y).y
    val map = (0 to maxYSensor).map(row => {
      (0 to maxYSensor).map(col => {
        if (sensors.contains(Point(row, col))) {
          "S"
        } else if (beacons.contains(Point(row, col))) {
          "B"
        } else if (scannedPoints.contains(Point(row, col))) {
          "#"
        } else {
          "."
        }
      }).mkString("")
    }).mkString("\n")
    println(map)

    val minX = 0
    val maxX = 20
    val minY = 0
    val maxY = 20

    // we need to find coordinates between min x and max x and min y max y
    // that are not a beacon nor sensor, nor are scanned
    println(solvePart2(sensorsWithBeacons, minX, maxX, minY, maxY))
  }

}
