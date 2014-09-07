package org.estewei.tsequence.data

import scalaz.{Need, Leibniz, Forall}
import Leibniz._

sealed abstract class TList[C[_, _], X, Y] {

  type UC[A] = { type λ[α] = (C[X, α], => TList[C, α, Y]) => A }

  def fold[A](n: (Y === X) => A, uc: Forall[UC[A]#λ]): A

}

object TList {
  private final case class TNil[C[_, _], X]() extends TList[C, X, X] {
    def fold[A](n: (X === X) => A, uc: Forall[UC[A]#λ]): A =
      n(refl[X])
  }

  private final case class TCons[C[_, _], X, Y, Z](h: C[X, Y], t: Need[TList[C, Y, Z]]) extends TList[C, X, Z] {
    def fold[A](n: (Z === X) => A, uc: Forall[UC[A]#λ]): A =
      uc[Y](h, t.value)
  }

  def tNil[C[_, _], X]: TList[C, X, X] =
    TNil()

  def tCons[C[_, _], X, Y, Z](h: C[X, Y], t: => TList[C, Y, Z]): TList[C, X, Z] =
    TCons(h, Need(t))

  implicit val tListTSeq: TSeq[TList] =
    new TSeq[TList] {

      def tempty[C[_, _], X]: TList[C, X, X] =
        tNil[C, X]

      def tsingleton[C[_, _], X, Y](x: C[X, Y]): TList[C, X, Y] =
        tCons(x, tNil)

      override def tcons[C[_, _], X, Y, Z](h: C[X, Y], t: => TList[C, Y, Z]): TList[C, X, Z] =
        tCons(h, t)

      override def tviewl[C[_, _], X, Y](s: TList[C, X, Y]): TViewL[TList, C, X, Y] =
        s fold (
          _.subst[({type λ[α] = TViewL[TList, C, α, Y]})#λ](TViewL.tEmptyL),
          new Forall[s.UC[TViewL[TList, C, X, Y]]#λ] {
            def apply[A] = (h, t) => TViewL.tUncons(h, t)
          })
    }
}

