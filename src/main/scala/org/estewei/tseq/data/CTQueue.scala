package org.estewei.tseq.data

import scala.sys
import scalaz.{Leibniz, Forall, ⊥, ⊤}
import Leibniz._

/** A purely functional catenable queue */
sealed abstract class CTQueue[Q[_[_, _], _, _], C[_, _], X, Y] {
  import CTQueue._

  type UQ[A] = { type λ[α] = (C[X, α], Q[CT[Q, C]#c, α, Y]) => A }

  def fold[A](z: (Y === X) => A, uq: Forall[UQ[A]#λ]): A

}

object CTQueue {
  import TViewL._

  type CT[Q[_[_, _], _, _], C[_, _]] = { type c[a, b] = CTQueue[Q, C, a, b] }
  type CTQ[Q[_[_, _], _, _]] = { type q[c[_, _], a, b] = CTQueue[Q, c, a, b] }

  private final case class C0[Q[_[_, _], _, _], C[_, _], X]() extends CTQueue[Q, C, X, X] {
    def fold[A](z: (X === X) => A, uq: Forall[UQ[A]#λ]): A =
      z(refl[X])
  }

  private final case class CN[Q[_[_, _], _, _], C[_, _], X, Y, Z](x: C[X, Y], q: Q[CT[Q, C]#c, Y, Z]) extends CTQueue[Q, C, X, Z] {
    def fold[A](z: (Z === X) => A, uq: Forall[UQ[A]#λ]): A =
      uq[Y](x, q)
  }

  def empty[Q[_[_, _], _, _], C[_, _], X]: CTQueue[Q, C, X, X] =
    C0()

  def singleton[Q[_[_, _], _, _], C[_, _], X, Y](x: C[X, Y])(implicit Q: TSeq[Q]): CTQueue[Q, C, X, Y] =
    CN(x, Q.tempty[CT[Q, C]#c, Y])

  implicit def ctQueueTSeq[Q[_[_, _], _, _]](implicit Q: TSeq[Q]): TSeq[CTQ[Q]#q] =
    new TSeq[CTQ[Q]#q] {

      def tempty[C[_, _], X]: CTQueue[Q, C, X, X] =
        empty[Q, C, X]

      def tsingleton[C[_, _], X, Y](x: C[X, Y]): CTQueue[Q, C, X, Y] =
        singleton[Q, C, X, Y](x)

      override def tconcat[C[_, _], X, Y, Z](a: CTQueue[Q, C, X, Y], b: => CTQueue[Q, C, Y, Z]): CTQueue[Q, C, X, Z] =
        a fold (
          e => e.subst[({type λ[α] = CTQueue[Q, C, α, Z]})#λ](b),
          new Forall[a.UQ[CTQueue[Q, C, X, Z]]#λ] {
            def apply[A] = (x, q) => {
              val bb = b
              bb fold (
                e => symm[⊥, ⊤, Z, Y](e).subst[({type λ[α] = CTQueue[Q, C, X, α]})#λ](a),
                new Forall[bb.UQ[CTQueue[Q, C, X, Z]]#λ] {
                  def apply[B] = (_, _) => CN(x, Q.tsnoc[CT[Q, C]#c, A, Y, Z](q, bb))
                })
            }
          })

      override def tviewl[C[_, _], X, Y](s: CTQueue[Q, C, X, Y]): TViewL[CTQ[Q]#q, C, X, Y] =
        s fold (
          e => e.subst[({type λ[α] = TViewL[CTQ[Q]#q, C, α, Y]})#λ](tEmptyL[CTQ[Q]#q, C, Y]),
          new Forall[s.UQ[TViewL[CTQ[Q]#q, C, X, Y]]#λ] {
            def apply[A] = (x, q) => tUncons[CTQ[Q]#q, C, X, A, Y](x, linkAll(q))
          })

    }

  ////

  private def linkAll[Q[_[_, _], _, _], C[_, _], A, B](v: Q[CT[Q, C]#c, A, B])(implicit Q: TSeq[Q]): CTQueue[Q, C, A, B] = {
    val vl = Q.tviewl[CT[Q, C]#c, A, B](v)
    vl fold (
      e => e.subst[({type λ[α] = CTQueue[Q, C, α, B]})#λ](empty[Q, C, B]),
      new Forall[vl.UC[CTQueue[Q, C, A, B]]#λ] {
        def apply[X] = (h, t) =>
          h fold (
            _ => sys.error("unpossible!"),
            new Forall[h.UQ[CTQueue[Q, C, A, B]]#λ] {
              def apply[Y] = (x, q) => CN(x, qsnoc(q, linkAll(t)))
            })
      })
  }

  private def qsnoc[Q[_[_, _], _, _], C[_, _], X, Y, Z](q: Q[CT[Q, C]#c, X, Y], r: CTQueue[Q, C, Y, Z])(implicit Q: TSeq[Q]): Q[CT[Q, C]#c, X, Z] =
    r fold (
      e => symm[⊥, ⊤, Z, Y](e).subst[({type λ[α] = Q[CT[Q, C]#c, X, α]})#λ](q),
      new Forall[r.UQ[Q[CT[Q, C]#c, X, Z]]#λ] {
        def apply[A] = (_, _) => Q.tsnoc[CT[Q, C]#c, X, Y, Z](q, r)
      })

}
