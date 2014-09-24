package scalaz

import org.estewei.tseq.fixed.MonadFree

object freeMonadFree {
  import Free._

  implicit def freeMF[S[_]]: MonadFree[Free, S] =
    new MonadFree[Free, S] {
      def point[A](a: => A) = Return(a)
      override def map[A, B](fa: Free[S, A])(f: A => B) = fa map f
      def bind[A, B](a: Free[S, A])(f: A => Free[S, B]) = a flatMap f
      def wrap[A](fa: S[Free[S, A]])(implicit S: Functor[S]): Free[S, A] = Suspend(fa)
      def resume[A](fa: Free[S, A])(implicit S: Functor[S]): S[Free[S, A]] \/ A = fa.resume
    }

}
