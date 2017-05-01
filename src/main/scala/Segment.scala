import scala.math.sqrt
import scala.math.pow

case class Point(name: String, x: Long, y: Long)

case class Segment(from: Point, to: Point) {
  def distance: Double = {
    sqrt(pow(from.x - to.x, 2) + pow(from.y - to.y, 2))
  }
}

case class Path(segments: Vector[Segment]) {
  def length: Double = {
    segments.map(_.distance).sum
  }

  def stops: Vector[Point] = {
    if (segments.length > 1)
      (segments.head.to +: new Path(segments.tail).stops).distinct
    else
      Vector()
  }
}
