package org.estewei.tsequence.data

import scalaz.{Leibniz, Forall}
import Leibniz._

sealed abstract class TSnocList[C[_, _], X, Y] {

  type US[A] = { type λ[α] = (TSnocList[C, X, α], C[α, Y]) => A }

  def fold[A](n: (X === Y) => A, us: Forall[US[A]#λ]): A

}

object TSnocList {
  private final case class SNil[C[_, _], X]() extends TSnocList[C, X, X] {
    def fold[A](n: (X === X) => A, us: Forall[US[A]#λ]): A =
      n(refl[X])
  }

  private final case class Snoc[C[_, _], X, Y, Z](p: TSnocList[C, X, Y], l: C[Y, Z]) extends TSnocList[C, X, Z] {
    def fold[A](n: (X === Z) => A, us: Forall[US[A]#λ]): A =
      us[Y](p, l)
  }

  def sNil[C[_, _], X]: TSnocList[C, X, X] =
    SNil()

  def tSnoc[C[_, _], X, Y, Z](p: TSnocList[C, X, Y], l: C[Y, Z]): TSnocList[C, X, Z] =
    Snoc(p, l)

  implicit val tSnocListTSeq: TSeq[TSnocList] =
    new TSeq[TSnocList] {

      def tempty[C[_, _], X]: TSnocList[C, X, X] =
        sNil[C, X]

      def tsingleton[C[_, _], X, Y](x: C[X, Y]): TSnocList[C, X, Y] =
        tSnoc(sNil, x)

      override def tsnoc[C[_, _], X, Y, Z](p: TSnocList[C, X, Y], l: C[Y, Z]): TSnocList[C, X, Z] =
        tSnoc(p, l)

      override def tviewr[C[_, _], X, Y](s: TSnocList[C, X, Y]): TViewR[TSnocList, C, X, Y] =
        s fold (
          _.subst[({type λ[α] = TViewR[TSnocList, C, X, α]})#λ](TViewR.tEmptyR),
          new Forall[s.US[TViewR[TSnocList, C, X, Y]]#λ] {
            def apply[A] = (p, l) => TViewR.tUnsnoc(p, l)
          })
    }
}
