package org.estewei.tsequence.data

import scalaz.{Category, Forall}

abstract class TSeq[S[_[_, _], _, _]] {
  import TViewL._, TViewR._

  def tempty[C[_, _], X]: S[C, X, X]

  def tsingleton[C[_, _], X, Y](x: C[X, Y]): S[C, X, Y]

  def tconcat[C[_, _], X, Y, Z](a: S[C, X, Y], b: => S[C, Y, Z]): S[C, X, Z] = {
    val vl = tviewl(a)
    vl fold (
      _.subst[({type λ[α] = S[C, α, Z]})#λ](b),
      new Forall[vl.UC[S[C, X, Z]]#λ] {
        def apply[A] = (h, t) => tcons(h, tconcat(t, b))
      })
  }

  def tviewl[C[_, _], X, Y](s: S[C, X, Y]): TViewL[S, C, X, Y] = {
    val vr = tviewr(s)
    vr fold (
      _.subst[({type λ[α] = TViewL[S, C, X, α]})#λ](tEmptyL),
      new Forall[vr.US[TViewL[S, C, X, Y]]#λ] {
        def apply[A] = (p, l) => {
          val vl = tviewl(p)
          vl fold (
            e => tUncons(e.subst[({type λ[α] = C[α, Y]})#λ](l), tempty[C, Y]),
            new Forall[vl.UC[TViewL[S, C, X, Y]]#λ] {
              def apply[B] = (h, t) => tUncons(h, tsnoc(t, l))
            })
        }
      })
  }

  def tuncons[C[_, _], X, Y](s: S[C, X, Y]): TViewL[S, C, X, Y] =
    tviewl(s)

  def tviewr[C[_, _], X, Y](s: S[C, X, Y]): TViewR[S, C, X, Y] = {
    val vl = tviewl(s)
    vl fold (
      _.subst[({type λ[α] = TViewR[S, C, α, Y]})#λ](tEmptyR),
      new Forall[vl.UC[TViewR[S, C, X, Y]]#λ] {
        def apply[A] = (h, t) => {
          val vr = tviewr(t)
          vr fold (
            e => tUnsnoc(tempty[C, X], e.subst[({type λ[α] = C[X, α]})#λ](h)),
            new Forall[vr.US[TViewR[S, C, X, Y]]#λ] {
              def apply[B] = (p, l) => tUnsnoc(tcons(h, p), l)
            })
        }
      })
  }

  def tunsnoc[C[_, _], X, Y](s: S[C, X, Y]): TViewR[S, C, X, Y] =
    tviewr(s)

  def tcons[C[_, _], X, Y, Z](h: C[X, Y], t: => S[C, Y, Z]): S[C, X, Z] =
    tconcat(tsingleton(h), t)

  def tsnoc[C[_, _], X, Y, Z](p: S[C, X, Y], l: C[Y, Z]): S[C, X, Z] =
    tconcat(p, tsingleton(l))

}

object TSeq {

  implicit def tSeqCategory[S[_[_, _], _, _], C[_, _]](implicit S: TSeq[S]): Category[({type l[x, y] = S[C, x, y]})#l] =
    new Category[({type l[x, y] = S[C, x, y]})#l] {
      def id[X]: S[C, X, X] =
        S.tempty[C, X]

      def compose[X, Y, Z](f: S[C, Y, Z], g: S[C, X, Y]): S[C, X, Z] =
        S.tconcat(g, f)
    }

}
