package maml.eval.directive

import maml.eval._
import maml.eval.tile._
import maml.ast._

import geotrellis.raster.Tile
import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._


object OpDirectives {
  val additionDirectiveDouble = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ + _))) })
  }

  val additionDirectiveInt = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ + _))) })
  }

  val additionDirectiveTile = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults
      .groupBy(_.kind)
    val dblRes: Interpreted[List[Double]] =
      grouped(MamlKind.Double)
        .map({ _.as[Double] })
        .toList.sequence
    val intRes: Interpreted[List[Int]] =
      grouped(MamlKind.Int)
        .map({ _.as[Int] })
        .toList.sequence
    val tileRes: Interpreted[List[LazyTile]] =
      grouped(MamlKind.Tile)
        .map({ _.as[LazyTile] })
        .toList.sequence

    (dblRes |@| intRes |@| tileRes).map({ case (dbls, ints, tiles) =>
      val tileSum = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ + _}, {_ + _}) })
      val scalarSum: Double = dbls.sum + ints.sum
      TileResult(LazyTile.DualMap(List(tileSum), { i: Int => i + scalarSum.toInt }, { i: Double => i + scalarSum }))
    })
  }

  val subtractionDirective = Directive { case (a@Subtraction(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (TileResult(lt1), TileResult(lt2)) => TileResult(LazyTile.DualCombine(List(lt1, lt2), {_ - _}, {_ - _}))
        case (IntResult(int), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {int - _}, {int.toDouble - _}))
        case (TileResult(lt1), IntResult(int)) => TileResult(LazyTile.DualMap(List(lt1), {_ - int}, {_ - int.toDouble}))
        case (DoubleResult(dbl), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {dbl.toInt - _}, {dbl - _}))
        case (TileResult(lt1), DoubleResult(dbl)) => TileResult(LazyTile.DualMap(List(lt1), {_ - dbl.toInt}, {_ - dbl}))
        case (IntResult(int1), IntResult(int2)) => IntResult(int1 - int2)
        case (DoubleResult(dbl1), DoubleResult(dbl2)) => DoubleResult(dbl1 - dbl2)
        case (IntResult(int), DoubleResult(dbl)) => DoubleResult(int - dbl)
        case (DoubleResult(dbl), IntResult(int)) => DoubleResult(dbl - int)
      }
    })
    Valid(results)
  }

  val divisionDirective = Directive { case (a@Subtraction(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (TileResult(lt1), TileResult(lt2)) => TileResult(LazyTile.DualCombine(List(lt1, lt2), {_ / _}, {_ / _}))
        case (IntResult(int), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {int / _}, {int.toDouble / _}))
        case (TileResult(lt), IntResult(int)) => TileResult(LazyTile.DualMap(List(lt), {_ / int}, {_ / int.toDouble}))
        case (DoubleResult(dbl), TileResult(lt)) => TileResult(LazyTile.DualMap(List(lt), {dbl.toInt / _}, {dbl / _}))
        case (TileResult(lt), DoubleResult(dbl)) => TileResult(LazyTile.DualMap(List(lt), {_ / dbl.toInt}, {_ / dbl}))
        case (IntResult(int1), IntResult(int2)) => IntResult(int1 / int2)
        case (DoubleResult(dbl1), DoubleResult(dbl2)) => DoubleResult(dbl1 / dbl2)
        case (IntResult(int), DoubleResult(dbl)) => DoubleResult(int / dbl)
        case (DoubleResult(dbl), IntResult(int)) => DoubleResult(dbl / int)
      }
    })
    Valid(results)
  }

  val multiplicationDirectiveDouble = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ * _))) })
  }
  val multiplicationDirectiveInt = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ * _))) })
  }
  val multiplicationDirectiveTile = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults
      .groupBy(_.kind)
    val dblRes: Interpreted[List[Double]] =
      grouped(MamlKind.Double)
        .map({ _.as[Double] })
        .toList.sequence
    val intRes: Interpreted[List[Int]] =
      grouped(MamlKind.Int)
        .map({ _.as[Int] })
        .toList.sequence
    val tileRes: Interpreted[List[LazyTile]] =
      grouped(MamlKind.Tile)
        .map({ _.as[LazyTile] })
        .toList.sequence

    (dblRes |@| intRes |@| tileRes).map({ case (dbls, ints, tiles) =>
      val tileSum = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ * _}, {_ * _}) })
      val scalarSum: Double = dbls.reduce(_ * _) + ints.reduce(_ * _)
      TileResult(LazyTile.DualMap(List(tileSum), { i: Int => i * scalarSum.toInt }, { i: Double => i * scalarSum }))
    })
  }

  val maxDirectiveDouble = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ max _))) })
  }
  val maxDirectiveInt = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ max _))) })
  }
  val maxDirectiveTile = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults
      .groupBy(_.kind)
    val dblRes: Interpreted[List[Double]] =
      grouped(MamlKind.Double)
        .map({ _.as[Double] })
        .toList.sequence
    val intRes: Interpreted[List[Int]] =
      grouped(MamlKind.Int)
        .map({ _.as[Int] })
        .toList.sequence
    val tileRes: Interpreted[List[LazyTile]] =
      grouped(MamlKind.Tile)
        .map({ _.as[LazyTile] })
        .toList.sequence

    (dblRes |@| intRes |@| tileRes).map({ case (dbls, ints, tiles) =>
      val tileMax = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ max _}, {_ max _}) })
      val scalarMax: Double = dbls.reduce(_ max _) + ints.reduce(_ max _)
      TileResult(LazyTile.DualMap(List(tileMax), { i: Int => i max scalarMax.toInt }, { i: Double => i max scalarMax }))
    })
  }

  val minDirectiveDouble = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ min _))) })
  }
  val minDirectiveInt = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ min _))) })
  }
  val minDirectiveTile = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults
      .groupBy(_.kind)
    val dblRes: Interpreted[List[Double]] =
      grouped(MamlKind.Double)
        .map({ _.as[Double] })
        .toList.sequence
    val intRes: Interpreted[List[Int]] =
      grouped(MamlKind.Int)
        .map({ _.as[Int] })
        .toList.sequence
    val tileRes: Interpreted[List[LazyTile]] =
      grouped(MamlKind.Tile)
        .map({ _.as[LazyTile] })
        .toList.sequence

    (dblRes |@| intRes |@| tileRes).map({ case (dbls, ints, tiles) =>
      val tileMin = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ min _}, {_ min _}) })
      val scalarMin: Double = dbls.reduce(_ min _) + ints.reduce(_ min _)
      TileResult(LazyTile.DualMap(List(tileMin), { i: Int => i min scalarMin.toInt }, { i: Double => i min scalarMin }))
    })
  }

  val lessThanDirective = Directive { case (a@Less(_), childResults) =>
    import geotrellis.raster.mapalgebra.local.Less.compare
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (TileResult(lt1), TileResult(lt2)) => TileResult(LazyTile.DualCombine(List(lt1, lt2), {(p: Int, q: Int) => if (compare(p, q)) 1 else 0}, {(p: Double, q: Double) => if (compare(p, q)) 1.0 else 0.0}))
        case (IntResult(int), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {p: Int => if (compare(int, p)) 1 else 0}, {p: Double => if (compare(int.toDouble, p)) 1.0 else 0.0}))
        case (TileResult(lt), IntResult(int)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, int)) 1 else 0}, {p: Double => if (compare(p, int.toDouble)) 1.0 else 0.0}))
        case (DoubleResult(dbl), TileResult(lt)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(dbl.toInt, p)) 1 else 0}, {p: Double => if (compare(dbl, p)) 1.0 else 0.0}))
        case (TileResult(lt), DoubleResult(dbl)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, dbl.toInt)) 1 else 0}, {p: Double => if (compare(p, dbl)) 1.0 else 0.0}))
        case (IntResult(int1), IntResult(int2)) => BoolResult(compare(int1, int2))
        case (DoubleResult(dbl1), DoubleResult(dbl2)) => BoolResult(compare(dbl1, dbl2))
        case (IntResult(int), DoubleResult(dbl)) => BoolResult(compare(int, dbl))
        case (DoubleResult(dbl), IntResult(int)) => BoolResult(compare(dbl, int))
      }
    })
    Valid(results)
  }

  val lessThanOrEqualToDirective = Directive { case (a@LessOrEqual(_), childResults) =>
    import geotrellis.raster.mapalgebra.local.LessOrEqual.compare
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (TileResult(lt1), TileResult(lt2)) => TileResult(LazyTile.DualCombine(List(lt1, lt2), {(p: Int, q: Int) => if (compare(p, q)) 1 else 0}, {(p: Double, q: Double) => if (compare(p, q)) 1.0 else 0.0}))
        case (IntResult(int), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {p: Int => if (compare(int, p)) 1 else 0}, {p: Double => if (compare(int.toDouble, p)) 1.0 else 0.0}))
        case (TileResult(lt), IntResult(int)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, int)) 1 else 0}, {p: Double => if (compare(p, int.toDouble)) 1.0 else 0.0}))
        case (DoubleResult(dbl), TileResult(lt)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(dbl.toInt, p)) 1 else 0}, {p: Double => if (compare(dbl, p)) 1.0 else 0.0}))
        case (TileResult(lt), DoubleResult(dbl)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, dbl.toInt)) 1 else 0}, {p: Double => if (compare(p, dbl)) 1.0 else 0.0}))
        case (IntResult(int1), IntResult(int2)) => BoolResult(compare(int1, int2))
        case (DoubleResult(dbl1), DoubleResult(dbl2)) => BoolResult(compare(dbl1, dbl2))
        case (IntResult(int), DoubleResult(dbl)) => BoolResult(compare(int, dbl))
        case (DoubleResult(dbl), IntResult(int)) => BoolResult(compare(dbl, int))
      }
    })
    Valid(results)
  }

  val equalToDirective = Directive { case (a@Equal(_), childResults) =>
    import geotrellis.raster.mapalgebra.local.Equal.compare
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (TileResult(lt1), TileResult(lt2)) => TileResult(LazyTile.DualCombine(List(lt1, lt2), {(p: Int, q: Int) => if (compare(p, q)) 1 else 0}, {(p: Double, q: Double) => if (compare(p, q)) 1.0 else 0.0}))
        case (IntResult(int), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {p: Int => if (compare(int, p)) 1 else 0}, {p: Double => if (compare(int.toDouble, p)) 1.0 else 0.0}))
        case (TileResult(lt), IntResult(int)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, int)) 1 else 0}, {p: Double => if (compare(p, int.toDouble)) 1.0 else 0.0}))
        case (DoubleResult(dbl), TileResult(lt)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(dbl.toInt, p)) 1 else 0}, {p: Double => if (compare(dbl, p)) 1.0 else 0.0}))
        case (TileResult(lt), DoubleResult(dbl)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, dbl.toInt)) 1 else 0}, {p: Double => if (compare(p, dbl)) 1.0 else 0.0}))
        case (IntResult(int1), IntResult(int2)) => BoolResult(compare(int1, int2))
        case (DoubleResult(dbl1), DoubleResult(dbl2)) => BoolResult(compare(dbl1, dbl2))
        case (IntResult(int), DoubleResult(dbl)) => BoolResult(compare(int, dbl))
        case (DoubleResult(dbl), IntResult(int)) => BoolResult(compare(dbl, int))
      }
    })
    Valid(results)
  }

  val greaterDirective = Directive { case (a@Greater(_), childResults) =>
    import geotrellis.raster.mapalgebra.local.Greater.compare
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (TileResult(lt1), TileResult(lt2)) => TileResult(LazyTile.DualCombine(List(lt1, lt2), {(p: Int, q: Int) => if (compare(p, q)) 1 else 0}, {(p: Double, q: Double) => if (compare(p, q)) 1.0 else 0.0}))
        case (IntResult(int), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {p: Int => if (compare(int, p)) 1 else 0}, {p: Double => if (compare(int.toDouble, p)) 1.0 else 0.0}))
        case (TileResult(lt), IntResult(int)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, int)) 1 else 0}, {p: Double => if (compare(p, int.toDouble)) 1.0 else 0.0}))
        case (DoubleResult(dbl), TileResult(lt)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(dbl.toInt, p)) 1 else 0}, {p: Double => if (compare(dbl, p)) 1.0 else 0.0}))
        case (TileResult(lt), DoubleResult(dbl)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, dbl.toInt)) 1 else 0}, {p: Double => if (compare(p, dbl)) 1.0 else 0.0}))
        case (IntResult(int1), IntResult(int2)) => BoolResult(compare(int1, int2))
        case (DoubleResult(dbl1), DoubleResult(dbl2)) => BoolResult(compare(dbl1, dbl2))
        case (IntResult(int), DoubleResult(dbl)) => BoolResult(compare(int, dbl))
        case (DoubleResult(dbl), IntResult(int)) => BoolResult(compare(dbl, int))
      }
    })
    Valid(results)
  }

  val greaterOrEqualDirective = Directive { case (a@GreaterOrEqual(_), childResults) =>
    import geotrellis.raster.mapalgebra.local.GreaterOrEqual.compare
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (TileResult(lt1), TileResult(lt2)) => TileResult(LazyTile.DualCombine(List(lt1, lt2), {(p: Int, q: Int) => if (compare(p, q)) 1 else 0}, {(p: Double, q: Double) => if (compare(p, q)) 1.0 else 0.0}))
        case (IntResult(int), TileResult(lt2)) => TileResult(LazyTile.DualMap(List(lt2), {p: Int => if (compare(int, p)) 1 else 0}, {p: Double => if (compare(int.toDouble, p)) 1.0 else 0.0}))
        case (TileResult(lt), IntResult(int)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, int)) 1 else 0}, {p: Double => if (compare(p, int.toDouble)) 1.0 else 0.0}))
        case (DoubleResult(dbl), TileResult(lt)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(dbl.toInt, p)) 1 else 0}, {p: Double => if (compare(dbl, p)) 1.0 else 0.0}))
        case (TileResult(lt), DoubleResult(dbl)) => TileResult(LazyTile.DualMap(List(lt), {p: Int => if (compare(p, dbl.toInt)) 1 else 0}, {p: Double => if (compare(p, dbl)) 1.0 else 0.0}))
        case (IntResult(int1), IntResult(int2)) => BoolResult(compare(int1, int2))
        case (DoubleResult(dbl1), DoubleResult(dbl2)) => BoolResult(compare(dbl1, dbl2))
        case (IntResult(int), DoubleResult(dbl)) => BoolResult(compare(int, dbl))
        case (DoubleResult(dbl), IntResult(int)) => BoolResult(compare(dbl, int))
      }
    })
    Valid(results)
  }
}

