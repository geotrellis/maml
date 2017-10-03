package com.azavea.maml.dsl.tile

import com.azavea.maml.eval.tile._

import geotrellis.raster._
import geotrellis.raster.mapalgebra.local._


trait LazyTileOperations {
  val self: LazyTile

  /** Simple local operations */
  def +(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Add.combine, Add.combine)
  def +(other: Int): LazyTile = LazyTile.DualMap(List(self), { Add.combine(_, other) }, { Add.combine(_, other) })
  def +:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Add.combine(_, other) }, { Add.combine(_, other) })
  def +(other: Double): LazyTile = LazyTile.DualMap(List(self), { Add.combine(_, d2i(other)) }, { Add.combine(_, other) })
  def +:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Add.combine(_, d2i(other)) }, { Add.combine(_, other) })

  def -(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Subtract.combine, Subtract.combine)
  def -(other: Int): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(_, other) }, { Subtract.combine(_, i2d(other)) })
  def -:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(_, other) }, { Subtract.combine(_, i2d(other)) })
  def -(other: Double): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(_, d2i(other)) }, { Subtract.combine(_, other) })
  def -:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Subtract.combine(_, d2i(other)) }, { Subtract.combine(_, other) })

  def *(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Multiply.combine, Multiply.combine)
  def *(other: Int): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(_, other) }, { Multiply.combine(_, i2d(other)) })
  def *:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(_, other) }, { Multiply.combine(_, i2d(other)) })
  def *(other: Double): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(_, d2i(other)) }, { Multiply.combine(_, other) })
  def *:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Multiply.combine(_, d2i(other)) }, { Multiply.combine(_, other) })

  def /(other: LazyTile): LazyTile = LazyTile.DualCombine(List(self, other), Divide.combine, Divide.combine)
  def /(other: Int): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(_, other) }, { Divide.combine(_, i2d(other)) })
  def /:(other: Int): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(_, other) }, { Divide.combine(_, i2d(other)) })
  def /(other: Double): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(_, d2i(other)) }, { Divide.combine(_, other) })
  def /:(other: Double): LazyTile = LazyTile.DualMap(List(self), { Divide.combine(_, d2i(other)) }, { Divide.combine(_, other) })

  /** Comparisons */
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

  /** Rounding Operations */
  def round: LazyTile = LazyTile.DualMap(List(self), identity, { z => if(isNoData(z)) z else math.round(z) })

  def floor: LazyTile = LazyTile.DualMap(List(self), identity, { z => if(isNoData(z)) z else math.floor(z) })

  def ceil: LazyTile = LazyTile.DualMap(List(self), identity, { z => if(isNoData(z)) z else math.ceil(z) })

  /** Rarer Arithmetic Operations */
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
}


