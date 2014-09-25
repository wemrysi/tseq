package org.estewei.tseq.fixed

import scala.{AnyVal, Function0}
import scala.annotation.tailrec
import scalaz.{Free => _, _}
import scalaz.std.function._

import org.estewei.tseq.data._

sealed abstract class Free[S[_], A] {
  import Free._
  import Leibniz._

  private[fixed] type X
  private[fixed] val h: View[S, X]
  private[fixed] val t: FMExp[S, X, A]

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => point(f(a)))

  final def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    FM(h, qs.tsnoc[FCAB[S]#c, X, A, B](t, FC(f)))

  final def >>=[B](f: A => Free[S, B]): Free[S, B] =
    flatMap(f)

  final def fold[B](r: A => B, s: S[Free[S, A]] => B)(implicit S: Functor[S]): B =
    resume.fold(s, r)

  final def resume(implicit S: Functor[S]): S[Free[S, A]] \/ A =
    toView match {
      case Return(a)  => \/.right(a)
      case Suspend(s) => \/.left(s)
    }

  final def mapSuspension[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): Free[T, A] =
    fold(point(_), s => suspend0(f(S.map(s)(_ mapSuspension f))))

  final def go(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): A = {
    @tailrec def go0(t: Free[S, A]): A =
      t.resume match {
        case -\/(s) => go0(f(s))
        case \/-(r) => r
      }
    go0(this)
  }

  final def run(implicit e: Free[S, A] === Trampoline[A]): A =
    e(this).go(_())

  final def runM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit S: Functor[S], M: Monad[M]): M[A] =
    fold(M.point(_), s => M.bind(f(s))(_ runM f))

  final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] =
    runM(f)

  ////

  @tailrec
  private final def toView(implicit S: Functor[S]): View[S, A] =
    h match {
      case Return(x) =>
        free(x) match {
          case \/-(a) => Return(a)
          case -\/(f) => f.toView
        }
      case Suspend(f) =>
        Suspend(S.map(f)(_ bindFree t))
    }

  private final def free(x: X): Free[S, A] \/ A = {
    val vl = qs.tviewl[FCAB[S]#c, X, A](t)

    vl.fold[Free[S, A] \/ A](
      e => \/.right(symm[⊥, ⊤, A, X](e)(x)),
      new Forall[vl.UC[Free[S, A] \/ A]#λ] {
        def apply[B] = (hc, tc) => \/.left(hc.run(x).bindFree(tc))
      })
  }

  private final def bindFree[B](e: FMExp[S, A, B]): Free[S, B] =
    FM(h, qs.tconcat[FCAB[S]#c, X, A, B](t, e))

}

object Free extends FreeInstances {

  /** A computation that can be stepped through, suspended, and paused */
  type Trampoline[A] = Free[Function0, A]

  def point[S[_], A](a: => A): Free[S, A] =
    fromView(Return(a))

  def suspend[S[_], A](fa: => Free[S, A])(implicit S: Applicative[S]): Free[S, A] =
    suspend0(S.point(fa))

  def return_[S[_], A](a: => A)(implicit S: Applicative[S]): Free[S, A] =
    suspend(point(a))

  def liftF[S[_], A](sa: S[A])(implicit S: Functor[S]): Free[S, A] =
    suspend0(S.map(sa)(point(_)))

  ////

  private[fixed] type FMExp[S[_], A, B] = FastTCQueue[FCAB[S]#c, A, B]
  private[fixed] type FCAB[S[_]] = { type c[a, b] = FC[S, a, b] }
  private[fixed] final case class FC[S[_], A, B](run: A => Free[S, B]) extends AnyVal

  private[fixed] sealed abstract class View[S[_], A]
  private final case class Return[S[_], A](a: A) extends View[S, A]
  private final case class Suspend[S[_], A](a: S[Free[S, A]]) extends View[S, A]

  private[fixed] def suspend0[S[_], A](sa: S[Free[S, A]]): Free[S, A] =
    fromView(Suspend(sa))

  private def fromView[S[_], A](v: View[S, A]): Free[S, A] =
    FM(v, qs.tempty[FCAB[S]#c, A])

  private def FM[S[_], A, B](x: View[S, A], y: FMExp[S, A, B]): Free[S, B] =
    new Free[S, B] { type X = A; val h = x; val t = y }

  private val qs = TSeq[FastTCQueue]
}

object Trampoline extends TrampolineInstances {
  import Free.{point, return_, Trampoline}

  def done[A](a: A): Trampoline[A] =
    point(a)

  def delay[A](a: => A): Trampoline[A] =
    return_(a)

  def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    Free.suspend(a)
}

sealed abstract class FreeInstances extends TrampolineInstances {
  import Free._

  implicit def freeMonadFree[S[_]]: MonadFree[Free, S] =
    new MonadFree[Free, S] {
      def point[A](a: => A) = Free.point(a)
      override def map[A, B](fa: Free[S, A])(f: A => B) = fa map f
      def bind[A, B](a: Free[S, A])(f: A => Free[S, B]) = a flatMap f
      def wrap[A](s: S[Free[S, A]])(implicit S: Functor[S]): Free[S, A] = suspend0(s)
      def resume[A](fa: Free[S, A])(implicit S: Functor[S]): S[Free[S, A]] \/ A = fa.resume
    }
}

sealed trait TrampolineInstances {
  import Free._

  implicit val trampolineInstance: Monad[Trampoline] with Comonad[Trampoline] =
    new Monad[Trampoline] with Comonad[Trampoline] {
      override def point[A](a: => A) = return_[Function0, A](a)
      def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta flatMap f
      def copoint[A](fa: Trampoline[A]) = fa.run
      def cobind[A, B](fa: Trampoline[A])(f: Trampoline[A] => B) = return_(f(fa))
      override def cojoin[A](fa: Trampoline[A]) = Free.point(fa)
    }
}
