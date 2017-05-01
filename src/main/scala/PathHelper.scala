
class PathHelper {

  def shortestPath(paths: Vector[Path]): Option[Path] =
    paths match {
      case Vector() => None
      case a +: z =>
        if (a == Path(Vector()) && z.isEmpty)
          None
        else if (z.isEmpty)
          Some(a)
        else if (z.head.length > a.length)
          shortestPath(a +: z.tail)
        else
          shortestPath(z)
    }

  def filterPathWithStop(paths: Vector[Path], stop: Point): Vector[Path] =
    paths match {
      case Vector() => Vector()
      case a +: z =>
        if (a.stops.contains(stop))
          a +: filterPathWithStop(z, stop)
        else
          filterPathWithStop(z, stop)
    }

  def filterPathWithStops(paths: Vector[Path], stops: Vector[Point]): Vector[Path] =
    stops match {
      case Vector() => paths
      case a +: z =>
        filterPathWithStop(paths, a).intersect(filterPathWithStops(paths, z))
    }

  def bestPathFromList(paths: Vector[Path], stops: Vector[Point]): Option[Path] =
    shortestPath(filterPathWithStops(paths, stops))

  def findAllPathsRec(segments: Vector[Segment], start: Point, end: Point, currentPath: Path): Vector[Path] =
    segments match {
      case Vector() => Vector(currentPath)
      case _ =>
        currentPath match {
          case Path(Vector()) =>
            if (segments.filter(_.from == start).isEmpty)
              Vector(currentPath)
            else
              segments.filter(_.from == start).flatMap(segment => findAllPathsRec(segments.filter(_ != segment), segment.to, end, new Path(currentPath.segments.:+(segment))))
          case _ =>
            if (currentPath.segments.last.to == end)
              Vector(currentPath)
            else if (segments.filter(seg => seg.from == start && seg.to != currentPath.segments.last.from).isEmpty)
              Vector(currentPath)
            else
              segments.filter(seg => seg.from == start && seg.to != currentPath.segments.last.from).flatMap(segment => findAllPathsRec(segments.filter(_ != segment), segment.to, end, new Path(currentPath.segments.:+(segment))))
        }
    }

  def findAllPaths(segments: Vector[Segment], start: Point, end: Point): Vector[Path] =
    segments match {
      case Vector() => Vector()
      case _ =>
        segments.flatMap(p => findAllPathsRec(segments, start, end, new Path(Vector()))).filter(_.segments.last.to == end).distinct
    }

  def bestPath(segments: Vector[Segment], from: Point, to: Point, stops: Vector[Point] = Vector()): Option[Path] =
    bestPathFromList(findAllPaths(segments, from, to), stops)

}