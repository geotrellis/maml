package maml.ast.utility

trait Neighborhood
case class Square(extent: Int) extends Neighborhood
case class Circle(radius: Double) extends Neighborhood
case class Nesw(extent: Int) extends Neighborhood
case class Wedge(radius: Double, startAngle: Double, endAngle: Double) extends Neighborhood
case class Annulus(innerRadius: Double, outerRadius: Double) extends Neighborhood

