import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import scala.math.abs

@RunWith(classOf[JUnitRunner])
class PathSpec extends FlatSpec with Matchers {

  val tl = Point("tl", 0, -10)
  val t = Point("t", 0, 0)
  val tr = Point("tr", 0, 10)
  val bl = Point("bl", 10, -10)
  val b = Point("b", 10, 0)
  val br = Point("br", 10, 20)

  // tl - t - tr
  //  |   |    |
  // bl - b --- br

  val points = Vector(tl, t, tr, bl, b, br)

  val listPoint = Vector(t, tr, br)
  val listPointUnique = Vector(t)

  val tlt = Segment(tl, t)
  val tlt2 = Segment(t, tl)
  val ttr = Segment(t, tr)
  val trbr = Segment(tr, br)
  val brb = Segment(br, b)
  val bltl = Segment(bl, tl)
  val tlbl = Segment(tl, bl)
  val tb = Segment(t, b)
  val bt = Segment(b, t)
  val topPath = Path(Vector(tlt, ttr))
  val blb = Segment(bl, b)
  val blb2 = Segment(b, bl)
  val bbr = Segment(b, br)
  val botPath = Path(Vector(blb, bbr))
  val trivial = Segment(tl, tl)
  val emptyPath = Path(Vector())
  val singlePath = Path(Vector(tlt))
  val emptyListPath = Vector()
  val topToBottom = Path(Vector(tlt, ttr, trbr, brb))
  val vectorEmptyPath = Vector(emptyPath)
  val listEmptyPath = Vector(emptyPath, emptyPath)
  val listSinglePath = Vector(topPath)
  val listSinglePath2 = Vector(botPath)
  val listPath = Vector(topPath, botPath)
  val listPath2 = Vector(topPath, botPath, topToBottom)
  val listSegment = Vector(tlt, tlt2, blb, blb2, bltl, tlbl, bt, tb)
  val tlTob1 = Path(Vector(tlt, tb))
  val tlTob2 = Path(Vector(tlbl, blb))
  val tlTotl1 = Path(Vector(tlt, tb, blb2, bltl))
  val tlTotl2 = Path(Vector(tlbl, blb, bt, tlt2))

  val helper = new PathHelper

  // Compute the length of a segment

  "a segment" should "compute its distance" in {
    val distance = tlt.distance
    distance shouldBe 10.0 +- 0.0001
  }

  "The length of a trivial segment" should "be 0" in {
    val distance = trivial.distance
    distance shouldBe 0
  }

  "The length of a vertical segment" should "be equal to abs(y1 - y2)" in {
    val distance = ttr.distance
    distance shouldBe abs(t.y - tr.y)
  }

  "The length of a general segment" should "be calculated" in {
    val distance = tlt.distance
    distance shouldBe 10.0 +- 0.0001
  }

  // Compute the length of a path

  "The length of an empty path" should "be equal to 0" in {
    val length = emptyPath.length
    length shouldBe 0
  }

  "The length of a path containing a single segment" should "be equal to the distance of this segment" in {
    val length = singlePath.length
    length shouldBe tlt.distance
  }

  "The length of a path containing at least two segments" should "be calculated" in {
    val length = botPath.length
    length shouldBe blb.distance + bbr.distance
  }

  // Enumerate all stops in a path

  "All stops off an empty path" should "have no stops" in {
    val stops = emptyPath.stops
    stops shouldBe Vector()
  }

  "All stops of a path containing a single segment" should "have no stops" in {
    val stops = singlePath.stops
    stops shouldBe Vector()
  }

  "All stops of path containing at least two segments" should "vector all those points except start and finish" in {
    val stops = topPath.stops
    stops shouldBe Vector(t)
  }

  // Find the shortest path in a list of paths

  "The shortest path of an empty list of paths" should "be None" in {
    val short = helper.shortestPath(emptyListPath)
    short shouldBe None
  }

  "The shortest path of a list of paths containing only empty paths" should "be None" in {
    val short = helper.shortestPath(listEmptyPath)
    short shouldBe None
  }

  "The shortest path of a list of paths containing a single path" should "be the single path" in {
    val short = helper.shortestPath(listSinglePath)
    short shouldBe Some(topPath)
  }

  "The shortest path of a list of paths containing at least two paths" should "be find the shortest path" in {
    val short = helper.shortestPath(listPath)
    short shouldBe Some(topPath)
  }

  // Filter paths keeping only those which include a stop

  "Filter empty list of paths for single stop" should "be Vector()" in {
    val pathWithStop = helper.filterPathWithStop(emptyListPath, t)
    pathWithStop shouldBe Vector()
  }

  "Filter list of paths not containing 1 stop" should "be Vector()" in {
    val pathWithStop = helper.filterPathWithStop(listSinglePath, b)
    pathWithStop shouldBe Vector()
  }

  "Filer list of paths containing at least one path with 1 stop" should "be find the path" in {
    val pathWithStop = helper.filterPathWithStop(listPath, t)
    pathWithStop shouldBe Vector(topPath)
  }

  // Filter paths keeping only those which include a given list of points

  "Filter empty list of paths for list of stops" should "be Vector()" in {
    val pathWithStops = helper.filterPathWithStops(emptyListPath, listPoint)
    pathWithStops shouldBe Vector()
  }

  "Filter list of paths not containing list of stops" should "be Vector()" in {
    val pathWithStops = helper.filterPathWithStops(listPath, listPoint)
    pathWithStops shouldBe Vector()
  }

  "Filer list of paths containing at least one path stopping at all stops" should "be find the path" in {
    val pathWithStops = helper.filterPathWithStops(listPath2, listPoint)
    pathWithStops shouldBe Vector(topToBottom)
  }

  // Find the best path

  "The best path of an empty list of paths passing throw a list of stops" should "be None" in {
    val bestPathFromList = helper.bestPathFromList(emptyListPath, listPointUnique)
    bestPathFromList shouldBe None
  }

  "The best path of a list of a single path not including list of stops" should "be None" in {
    val bestPathFromList = helper.bestPathFromList(listSinglePath2, listPointUnique)
    bestPathFromList shouldBe None
  }

  "The best path of a list of a single path including the list of stops" should "be the single path" in {
    val bestPathFromList = helper.bestPathFromList(listSinglePath, listPointUnique)
    bestPathFromList shouldBe Some(topPath)
  }

  "The best path of a list of paths including the list of stops" should "be find the best path from the list" in {
    val bestPathFromList = helper.bestPathFromList(listPath2, listPointUnique)
    bestPathFromList shouldBe Some(topPath)
  }

  // Find all possible paths

  "The possible paths when the segments are empty" should "be Vector()" in {
    val all = helper.findAllPathsRec(emptyListPath, tl, br)
    all shouldBe Vector()
  }

  "The possible paths with the list `[Segment(tl, t)]`" should "be the segment" in {
    val all = helper.findAllPathsRec(Vector(tlt), tl, t)
    all shouldBe Vector(singlePath)
  }

  "The possible paths with segments without cycle" should "be the 2 paths" in {
    val all = helper.findAllPathsRec(listSegment, tl, b)
    all shouldBe Vector(tlTob1, tlTob2)
  }

  "The possible paths with segments with cycle" should "be the path" in {
    val all = helper.findAllPathsRec(listSegment, tl, tl)
    all shouldBe Vector(tlTotl1, tlTotl2)
  }

  // Wrap-up

  "This magnificent and practical code" should "find the best path" in {
    val segments = Vector(
      Segment(tl, tr),
      Segment(tr, br),
      Segment(tl, bl),
      Segment(bl, br))

    helper.bestPath(segments, tl, br).get shouldBe Path(Vector(
      Segment(tl, tr),
      Segment(tr, br)))
  }

  it should "find the best path with a stop" in {
    val segments = Vector(
      Segment(tl, tr),
      Segment(tr, br),
      Segment(tl, bl),
      Segment(bl, br))

    helper.bestPath(segments, tl, br, Vector(bl)).get shouldBe Path(Vector(
      Segment(tl, bl),
      Segment(bl, br)))
  }

}
