package com.azavea.maml

package object compact {
  type ParallelCompact[M[_], F[_]] = cats.Parallel.Aux[M, F]
}
