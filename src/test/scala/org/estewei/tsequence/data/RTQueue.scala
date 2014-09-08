package org.estewei.tsequence.data

import scalaz.{Forall, Leibniz, ⊥, ⊤}

/** Queue with O(1) worst-case operations */
sealed abstract class RTQueue[C[_, _], A, B]

object RTQueue {
  import TConsList._
  import TSnocList._

  private final case class RQ[C[_, _], X, Y, Z, A](
    f: TConsList[C, X, Y],
    r: TSnocList[C, Y, Z],
    a: TConsList[C, A, Y]
  ) extends RTQueue[C, X, Z]

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

  private def revAppend[C[_, _], X, Y, Z](l: TConsList[C, X, Y], r: TSnocList[C, Y, Z]): TConsList[C, X, Z] =
    rotate(l, r, cNil[C, Z])

  private def rotate[C[_, _], W, X, Y, Z](
    f: TConsList[C, W, X],
    a: TSnocList[C, X, Y],
    r: TConsList[C, Y, Z]
   ): TConsList[C, W, Z] = {
    def err = scala.sys.error("Invariant |a| = |f| - (|r| - 1) broken!")

    f fold (
      e1 => {
        a fold (
          _ => err,
          new Forall[a.US[TConsList[C, W, Z]]#λ] {
            def apply[A] = (p, l) =>
              p fold (
                e2 => e1.compose(Leibniz.symm[⊥, ⊤, X, A](e2)).subst[({type λ[α] = TConsList[C, α, Z]})#λ](tCons(l, r)),
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
