package org.estewei.tseq.fixed

import scala.inline
import scalaz._

trait MonadFree[M[_[_], _], S[_]] extends Monad[({type λ[α] = M[S, α]})#λ] {

  def wrap[A](sa: S[M[S, A]])(implicit S: Functor[S]): M[S, A]

  def resume[A](ma: M[S, A])(implicit S: Functor[S]): S[M[S, A]] \/ A

  final def suspend[A](m: => M[S, A])(implicit S: Applicative[S]): M[S, A] =
    wrap(S.point(m))

  final def return_[A](a: => A)(implicit S: Applicative[S]): M[S, A] =
    suspend(point(a))

  final def liftF[A](sa: S[A])(implicit S: Functor[S]): M[S, A] =
    wrap(S.map(sa)(point(_)))

}

object MonadFree {
  @inline def apply[M[_[_], _], S[_]](implicit MF: MonadFree[M, S]): MonadFree[M, S] = MF
}
