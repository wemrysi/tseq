package org.estewei.tsequence.data

import scala.sys
import scalaz.{Forall, Leibniz, ⊥, ⊤}

/** Queue with O(1) worst-case operations */
sealed abstract class RTQueue[C[_, _], A, B] {
  type Y
  type X
  private[data] val f: TConsList[C, A, Y]
  private[data] val r: TSnocList[C, Y, B]
  private[data] val a: TConsList[C, X, Y]
}

object RTQueue {
  import TConsList._
  import TSnocList._
  import Leibniz._

  def empty[C[_, _], A]: RTQueue[C, A, A] =
    RQ(cNil[C, A], sNil[C, A], cNil[C, A])

  def apply[C[_, _], X, Y, Z, A](
    f: TConsList[C, X, Y],
    r: TSnocList[C, Y, Z],
    a: TConsList[C, A, Y]
  ): RTQueue[C, X, Z] =
    a fold (
      e => { val x = revAppend(f, r); RQ(x, sNil[C, Z], x) },
      new Forall[a.UC[RTQueue[C, X, Z]]#λ] {
        def apply[B] = (_, t) =>
          RQ(f, r, t)
      })

  implicit val rtQueueTSeq: TSeq[RTQueue] =
    new TSeq[RTQueue] {

      def tempty[C[_, _], X]: RTQueue[C, X, X] =
        empty[C, X]

      def tsingleton[C[_, _], X, Y](x: C[X, Y]): RTQueue[C, X, Y] = {
        val c = tConsListTSeq.tsingleton(x)
        RQ(c, sNil[C, Y], c)
      }

      override def tsnoc[C[_, _], X, Y, Z](p: RTQueue[C, X, Y], l: C[Y, Z]): RTQueue[C, X, Z] =
        RTQueue(p.f, p.r.tsnoc(l), p.a)

      override def tviewl[C[_, _], A, B](q: RTQueue[C, A, B]): TViewL[RTQueue, C, A, B] =
        q.f fold (
          e1 => q.r fold (
            e2 =>
              e2.subst[({type λ[α] = TViewL[RTQueue, C, A, α]})#λ](
                e1.subst[({type λ[α] = TViewL[RTQueue, C, α, q.Y]})#λ](TViewL.tEmptyL[RTQueue, C, q.Y])),
            new Forall[q.r.US[TViewL[RTQueue, C, A, B]]#λ] {
              def apply[U] = (_, _) =>
                sys.error("unpossible!")
            }),
          new Forall[q.f.UC[TViewL[RTQueue, C, A, B]]#λ] {
            def apply[V] = (h, t) =>
              TViewL.tUncons(h, RTQueue(t, q.r, q.a))
          })

    }

  ////

  private def RQ[C[_, _], A, B, E, D](
    _f: TConsList[C, A, E],
    _r: TSnocList[C, E, B],
    _a: TConsList[C, D, E]
  ): RTQueue[C, A, B] =
    new RTQueue[C, A, B] {
      type Y = E
      type X = D
      val f = _f
      val r = _r
      val a = _a
    }

  private def revAppend[C[_, _], X, Y, Z](l: TConsList[C, X, Y], r: TSnocList[C, Y, Z]): TConsList[C, X, Z] =
    rotate(l, r, cNil[C, Z])

  private def rotate[C[_, _], W, X, Y, Z](
    f: TConsList[C, W, X],
    a: TSnocList[C, X, Y],
    r: TConsList[C, Y, Z]
   ): TConsList[C, W, Z] = {
    def err = sys.error("Invariant |a| = |f| - (|r| - 1) broken!")

    f fold (
      e1 => {
        a fold (
          _ => err,
          new Forall[a.US[TConsList[C, W, Z]]#λ] {
            def apply[A] = (p, l) =>
              p fold (
                e2 => e1.compose(symm[⊥, ⊤, X, A](e2)).subst[({type λ[α] = TConsList[C, α, Z]})#λ](tCons(l, r)),
                new Forall[p.US[TConsList[C, W, Z]]#λ] {
                  def apply[B] = (_, _) =>
                    err
                })
          })
      },
      new Forall[f.UC[TConsList[C, W, Z]]#λ] {
        def apply[A] = (h, t) =>
          a fold (
            _ => err,
            new Forall[a.US[TConsList[C, W, Z]]#λ] {
              def apply[B] = (p, l) =>
                tCons(h, rotate(t, p, tCons(l, r)))
            })
      })
  }

}
