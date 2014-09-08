package org.estewei.tseq.data

import scalaz.{Leibniz, Forall}
import Leibniz._

sealed abstract class TViewR[S[_[_, _], _, _], C[_, _], X, Y] {

  type US[A] = { type λ[α] = (S[C, X, α], C[α, Y]) => A }

  /** HT: @pchiusano for the insight on how to define fold! */
  def fold[A](e: (X === Y) => A, us: Forall[US[A]#λ]): A

}

object TViewR {
  private final case class TEmptyR[S[_[_, _], _, _], C[_, _], X]() extends TViewR[S, C, X, X] {
    def fold[A](e: (X === X) => A, us: Forall[US[A]#λ]): A =
      e(refl[X])
  }

  private final case class TUnsnoc[S[_[_, _], _, _], C[_, _], X, Y, Z](p: S[C, X, Y], l: C[Y, Z]) extends TViewR[S, C, X, Z] {
    def fold[A](e: (X === Z) => A, us: Forall[US[A]#λ]): A =
      us[Y](p, l)
  }

  def tEmptyR[S[_[_, _], _, _], C[_, _], X]: TViewR[S, C, X, X] =
    TEmptyR()

  def tUnsnoc[S[_[_, _], _, _], C[_, _], X, Y, Z](p: S[C, X, Y], l: C[Y, Z]): TViewR[S, C, X, Z] =
    TUnsnoc(p, l)

}
