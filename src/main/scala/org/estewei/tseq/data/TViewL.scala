package org.estewei.tseq.data

import scalaz.{Need, Leibniz, Forall}
import Leibniz._

sealed abstract class TViewL[S[_[_, _], _, _], C[_, _], X, Y] {

  type UC[A] = { type λ[α] = (C[X, α], => S[C, α, Y]) => A }

  /** HT: @pchiusano for the insight on how to define fold! */
  def fold[A](e: (Y === X) => A, uc: Forall[UC[A]#λ]): A

}

object TViewL {
  private final case class TEmptyL[S[_[_, _], _, _], C[_, _], X]() extends TViewL[S, C, X, X] {
    def fold[A](e: (X === X) => A, uc: Forall[UC[A]#λ]): A =
      e(refl[X])
  }

  private final case class TUncons[S[_[_, _], _, _], C[_, _], X, Y, Z](h: C[X, Y], t: Need[S[C, Y, Z]]) extends TViewL[S, C, X, Z] {
    def fold[A](e: (Z === X) => A, uc: Forall[UC[A]#λ]): A =
      uc[Y](h, t.value)
  }

  def tEmptyL[S[_[_, _], _, _], C[_, _], X]: TViewL[S, C, X, X] =
    TEmptyL()

  def tUncons[S[_[_, _], _, _], C[_, _], X, Y, Z](h: C[X, Y], t: => S[C, Y, Z]): TViewL[S, C, X, Z] =
    TUncons(h, Need(t))

}
