    final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double    = ???
    override def y: Double    = ???
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
  }

  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded =
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

      // if needed, fix the code to be correct
      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.minX).min
      override def minY: Double = objects.map(_.minX).min
      override def maxY: Double = objects.map(_.minX).min
    }
  
  // Homework by 2021-02-03
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.
