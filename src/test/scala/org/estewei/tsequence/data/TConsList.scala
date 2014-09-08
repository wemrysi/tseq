package org.estewei.tsequence.data

import scalaz.{Need, Leibniz, Forall}
import Leibniz._

sealed abstract class TConsList[C[_, _], X, Y] {
  import TConsList._

  type UC[A] = { type λ[α] = (C[X, α], => TConsList[C, α, Y]) => A }

  def fold[A](n: (Y === X) => A, uc: Forall[UC[A]#λ]): A

  def tcons[W](x: C[W, X]): TConsList[C, W, Y] =
    tCons(x, this)

}

object TConsList {
  private final case class CNil[C[_, _], X]() extends TConsList[C, X, X] {
    def fold[A](n: (X === X) => A, uc: Forall[UC[A]#λ]): A =
      n(refl[X])
  }

  private final case class Cons[C[_, _], X, Y, Z](h: C[X, Y], t: Need[TConsList[C, Y, Z]]) extends TConsList[C, X, Z] {
    def fold[A](n: (Z === X) => A, uc: Forall[UC[A]#λ]): A =
      uc[Y](h, t.value)
  }

  def cNil[C[_, _], X]: TConsList[C, X, X] =
    CNil()

  def tCons[C[_, _], X, Y, Z](h: C[X, Y], t: => TConsList[C, Y, Z]): TConsList[C, X, Z] =
    Cons(h, Need(t))

  implicit val tConsListTSeq: TSeq[TConsList] =
    new TSeq[TConsList] {

      def tempty[C[_, _], X]: TConsList[C, X, X] =
        cNil[C, X]

      def tsingleton[C[_, _], X, Y](x: C[X, Y]): TConsList[C, X, Y] =
        tCons(x, cNil)

      override def tcons[C[_, _], X, Y, Z](h: C[X, Y], t: => TConsList[C, Y, Z]): TConsList[C, X, Z] =
        tCons(h, t)

      override def tviewl[C[_, _], X, Y](s: TConsList[C, X, Y]): TViewL[TConsList, C, X, Y] =
        s fold (
          _.subst[({type λ[α] = TViewL[TConsList, C, α, Y]})#λ](TViewL.tEmptyL),
          new Forall[s.UC[TViewL[TConsList, C, X, Y]]#λ] {
            def apply[A] = (h, t) => TViewL.tUncons(h, t)
          })
    }
}

