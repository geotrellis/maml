package com.azavea.maml.dsl.tile

import com.azavea.maml.eval.tile._

import geotrellis.raster._
import geotrellis.raster.render.BreakMap
import geotrellis.raster.mapalgebra.local._


trait LazyTileOperations {
  val self: LazyTile

  /** Arithmetic Operations*/
  def +(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Add.combine, Add.combine)
  def +(other: Int): LazyTile = LazyTile.DualMap(List(self), { Add.combine(_, other) }, { Add.combine(_, other) })
  def +:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Add.combine(other, _) }, { Add.combine(other, _) })
  def +(other: Double): LazyTile = LazyTile.DualMap(List(self), { Add.combine(_, d2i(other)) }, { Add.combine(_, other) })
  def +:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Add.combine(d2i(other), _) }, { Add.combine(other, _) })

  def -(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Subtract.combine, Subtract.combine)
  def -(other: Int): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(_, other) }, { Subtract.combine(_, i2d(other)) })
  def -:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(other, _) }, { Subtract.combine(i2d(other), _) })
  def -(other: Double): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(_, d2i(other)) }, { Subtract.combine(_, other) })
  def -:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(d2i(other), _) }, { Subtract.combine(other, _) })

  def *(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Multiply.combine, Multiply.combine)
  def *(other: Int): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(_, other) }, { Multiply.combine(_, i2d(other)) })
  def *:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(other, _) }, { Multiply.combine(i2d(other), _) })
  def *(other: Double): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(_, d2i(other)) }, { Multiply.combine(_, other) })
  def *:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(d2i(other), _) }, { Multiply.combine(other, _) })

  def /(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Divide.combine, Divide.combine)
  def /(other: Int): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(_, other) }, { Divide.combine(_, i2d(other)) })
  def /:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(other, _) }, { Divide.combine(i2d(other), _) })
  def /(other: Double): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(_, d2i(other)) }, { Divide.combine(_, other) })
  def /:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(d2i(other), _) }, { Divide.combine(other, _) })

  def **(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Pow.combine, Pow.combine)
  def **(other: Int): LazyTile = LazyTile.DualMap(List(self), { Pow.combine(_, other) }, { Pow.combine(_, i2d(other)) })
  def **:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Pow.combine(other, _) }, { Pow.combine(i2d(other), _) })
  def **(other: Double): LazyTile = LazyTile.DualMap(List(self), { Pow.combine(_, d2i(other)) }, { Pow.combine(_, other) })
  def **:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Pow.combine(d2i(other), _) }, { Pow.combine(other, _) })


  def logE: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.log(i2d(z))) },
    { z => if(isNoData(z)) z else math.log(z) }
  )

  def log10: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.log10(i2d(z))) },
    { z => if(isNoData(z)) z else math.log10(z) }
  )

  def sqrt: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.sqrt(i2d(z))) },
    { z => if(isNoData(z)) z else math.sqrt(z) }
  )

  def abs: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else math.abs(z) },
    { z => if(isNoData(z)) z else math.abs(z) }
  )

  def isDefined: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if (isData(z)) 1 else 0 },
    { z => if (isData(z)) 1.0 else 0.0 }
  )

  def isUndefined: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if (isNoData(z)) 1 else 0 },
    { z => if (isNoData(z)) 1.0 else 0.0 }
  )

  def pow(i: Int): LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if (isNoData(z)) 1 else 0 },
    { z => if (isNoData(z)) 1.0 else 0.0 }
  )

  def pow(d: Double): LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if (isNoData(z)) 1 else 0 },
    { z => if (isNoData(z)) 1.0 else 0.0 }
  )

  def changeSign: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if (isNoData(z)) z else z * -1 },
    { z => if (isNoData(z)) z else z * - 1 }
  )

  /** Numeric Comparisons */
  def <(other: LazyTile): LazyTile =
    LazyTile.DualCombine(List(self, other),
      {(i1: Int, i2: Int) => if (Less.compare(i1, i2)) 1 else 0 },
      {(d1: Double, d2: Double) => if (Less.compare(d1, d2)) 1.0 else 0.0 }
    )
  def <(other: Int): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Less.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Less.compare(d, other)) 1.0 else 0.0 }
    )
  def <(other: Double): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Less.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Less.compare(d, other)) 1.0 else 0.0 }
    )

  def <=(other: LazyTile): LazyTile =
    LazyTile.DualCombine(List(self, other),
      {(i1: Int, i2: Int) => if (LessOrEqual.compare(i1, i2)) 1 else 0 },
      {(d1: Double, d2: Double) => if (LessOrEqual.compare(d1, d2)) 1.0 else 0.0 }
    )
  def <=(other: Int): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (LessOrEqual.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (LessOrEqual.compare(d, other)) 1.0 else 0.0 }
    )
  def <=(other: Double): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (LessOrEqual.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (LessOrEqual.compare(d, other)) 1.0 else 0.0 }
    )

  def ===(other: LazyTile): LazyTile =
    LazyTile.DualCombine(List(self, other),
      {(i1: Int, i2: Int) => if (Equal.compare(i1, i2)) 1 else 0 },
      {(d1: Double, d2: Double) => if (Equal.compare(d1, d2)) 1.0 else 0.0 }
    )
  def ===(other: Int): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Equal.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Equal.compare(d, other)) 1.0 else 0.0 }
    )
  def ===(other: Double): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Equal.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Equal.compare(d, other)) 1.0 else 0.0 }
    )

  def !==(other: LazyTile): LazyTile =
    LazyTile.DualCombine(List(self, other),
      {(i1: Int, i2: Int) => if (Unequal.compare(i1, i2)) 1 else 0 },
      {(d1: Double, d2: Double) => if (Unequal.compare(d1, d2)) 1.0 else 0.0 }
    )
  def !==(other: Int): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Unequal.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Unequal.compare(d, other)) 1.0 else 0.0 }
    )
  def !==(other: Double): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Unequal.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Unequal.compare(d, other)) 1.0 else 0.0 }
    )

  def >=(other: LazyTile): LazyTile =
    LazyTile.DualCombine(List(self, other),
      {(i1: Int, i2: Int) => if (GreaterOrEqual.compare(i1, i2)) 1 else 0 },
      {(d1: Double, d2: Double) => if (GreaterOrEqual.compare(d1, d2)) 1.0 else 0.0 }
    )
  def >=(other: Int): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (GreaterOrEqual.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (GreaterOrEqual.compare(d, other)) 1.0 else 0.0 }
    )
  def >=(other: Double): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (GreaterOrEqual.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (GreaterOrEqual.compare(d, other)) 1.0 else 0.0 }
    )

  def >(other: LazyTile): LazyTile =
    LazyTile.DualCombine(List(self, other),
      {(i1: Int, i2: Int) => if (Greater.compare(i1, i2)) 1 else 0 },
      {(d1: Double, d2: Double) => if (Greater.compare(d1, d2)) 1.0 else 0.0 }
    )
  def >(other: Int): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Greater.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Greater.compare(d, other)) 1.0 else 0.0 }
    )
  def >(other: Double): LazyTile =
    LazyTile.DualMap(List(self),
      {(i: Int) => if (Greater.compare(i, other.toInt)) 1 else 0},
      {(d: Double) => if (Greater.compare(d, other)) 1.0 else 0.0 }
    )

  /** Trigonometric Operations */
  def sin: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.sin(z)) },
    { z => if(isNoData(z)) z else math.sin(z) }
  )
  def cos: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.cos(z)) },
    { z => if(isNoData(z)) z else math.cos(z) }
  )
  def tan: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.tan(z)) },
    { z => if(isNoData(z)) z else math.tan(z) }
  )

  def sinh: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.sinh(z)) },
    { z => if(isNoData(z)) z else math.sinh(z) }
  )
  def cosh: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.cosh(z)) },
    { z => if(isNoData(z)) z else math.cosh(z) }
  )
  def tanh: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.tanh(z)) },
    { z => if(isNoData(z)) z else math.tanh(z) }
  )

  def asin: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.asin(z)) },
    { z => if(isNoData(z)) z else math.asin(z) }
  )
  def acos: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.acos(z)) },
    { z => if(isNoData(z)) z else math.acos(z) }
  )
  def atan: LazyTile = LazyTile.DualMap(List(self),
    { z: Int => if(isNoData(z)) z else d2i(math.atan(z)) },
    { z => if(isNoData(z)) z else math.atan(z) }
  )

  def atan2(other: LazyTile) = LazyTile.DualCombine(List(self, other), { (z1, z2) => d2i(math.atan2(i2d(z1), i2d(z2))) }, { (z1, z2) => math.atan2(z1, z2) })
  def atan2(other: Int): LazyTile = LazyTile.DualMap(List(self), { i: Int => d2i(math.atan2(i, other)) }, { math.atan2(_, other) })
  def atan2(other: Double): LazyTile = LazyTile.DualMap(List(self), { i: Int => d2i(math.atan2(i, other)) }, { math.atan2(_, other) })

  /** Rounding Operations */
  def round: LazyTile = LazyTile.DualMap(List(self), identity, { z => if(isNoData(z)) z else math.round(z) })

  def floor: LazyTile = LazyTile.DualMap(List(self), identity, { z => if(isNoData(z)) z else math.floor(z) })

  def ceil: LazyTile = LazyTile.DualMap(List(self), identity, { z => if(isNoData(z)) z else math.ceil(z) })

  /** Logical Operations */
  // TODO: Look into GT implementations for logical operations...
  //       The handling of nodata vs 0 vs false is not obvious
  def &&(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), And.combine, And.combine)
  def &&(other: Int): LazyTile = LazyTile.DualMap(List(self), { And.combine(_, other) }, { And.combine(_, other) })
  def &&:(other: Int): LazyTile = LazyTile.DualMap(List(self), { And.combine(_, other) }, { And.combine(_, other) })
  def &&(other: Double): LazyTile = LazyTile.DualMap(List(self), { And.combine(_, d2i(other)) }, { And.combine(_, other) })
  def &&:(other: Double): LazyTile = LazyTile.DualMap(List(self), { And.combine(_, d2i(other)) }, { And.combine(_, other) })

  def ||(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Or.combine, Or.combine)
  def ||(other: Int): LazyTile = LazyTile.DualMap(List(self), { Or.combine(_, other) }, { Or.combine(_, other) })
  def ||:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Or.combine(_, other) }, { Or.combine(_, other) })
  def ||(other: Double): LazyTile = LazyTile.DualMap(List(self), { Or.combine(_, d2i(other)) }, { Or.combine(_, other) })
  def ||:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Or.combine(_, d2i(other)) }, { Or.combine(_, other) })

  def xor(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Xor.combine, Xor.combine)
  def xor(other: Int): LazyTile = LazyTile.DualMap(List(self), { Xor.combine(_, other) }, { Xor.combine(_, other) })
  def xor(other: Double): LazyTile = LazyTile.DualMap(List(self), { Xor.combine(_, d2i(other)) }, { Xor.combine(_, other) })

  def not: LazyTile = LazyTile.DualMap(List(self), { z => if (isNoData(z)) z else if (z == 0) 1 else 0 }, { z => if (isNoData(z)) z else if (z == 0.0) 1.0 else 0.0 })

  /** Tile specific methods */
  def classify(breaks: BreakMap[Double, Int]) = LazyTile.DualMap(List(self), { i => breaks(i2d(i)) }, { d => i2d(breaks(d)) })

}

