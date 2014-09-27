package org.estewei.tseq.fixed

import scala.{Int, Function0, AnyVal}
import scala.{Stream, Nil, ::, List}
import scala.Predef.intWrapper
import scala.annotation.tailrec

import org.scalameter.api._
import scalaz.{Free => ZFree, Trampoline => _, _}
import scalaz.iteratee._

object TrampolineBenchmark extends PerformanceTest {
  import freeMonadFree._
  import scalaz.std.function._
  import scalaz.syntax.bind._

  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  def ackermann[M[_[_], _]](m: Int, n: Int)(implicit M: MonadFree[M, Function0]): M[Function0, Int] = {
    if (m <= 0)
      M.point(n + 1)
    else if (n <= 0)
      M.suspend(ackermann[M](m - 1, 1))
    else for {
      a <- M.suspend(ackermann[M](m, n - 1))
      b <- M.suspend(ackermann[M](m - 1, a))
    } yield b
  }

  // Ackermann

  val m = 3
  val ns = Gen.range("n")(1, 7, 1)

  performance of "TSeq Trampoline: ackermann" in {
    using(ns) in { n =>
      ackermann[Free](m, n).run
    }
  }

  performance of "Scalaz Trampoline: ackermann" in {
    using(ns) in { n =>
      ackermann[ZFree](m, n).run
    }
  }

  // Iteratee Usage

  def iter[F[_]: Applicative] = Iteratee.fold[Int, F, Int](0){ case (a, v) => a + v }
  def enum[F[_]: Monad](max: Int) = EnumeratorT.enumStream[Int, F](Stream.fill(max)(1))

  val size = Gen.range("size")(10000, 50000, 10000)

  performance of "TSeq Iteratee" in {
    using(size) in { s =>
      (iter[Trampoline] &= enum[Trampoline](s)).run.run
    }
  }

  performance of "Scalaz Iteratee" in {
    using(size) in { s =>
      (iter[ZFree.Trampoline] &= enum[ZFree.Trampoline](s)).run.run
    }
  }

  // Improvements over free for left-associated binds

  final case class Get[I, O](f: I => O) extends AnyVal

  type G[I] = { type O[o] = Get[I, o] }

  implicit def getFunctor[I]: Functor[G[I]#O] =
    new Functor[G[I]#O] {
      def map[A, B](g: Get[I, A])(f: A => B): Get[I, B] =
        Get(f compose g.f)
    }

  type It[M[_[_], _]] = { type f[i, o] = M[G[i]#O, o] }

  def get[M[_[_], _], I](implicit M: MonadFree[M, G[I]#O]): It[M]#f[I, I] =
    M.liftF(Get(i => i))

  def addGet[M[_[_], _]](x: Int)(implicit M: MonadFree[M, G[Int]#O]): It[M]#f[Int, Int] =
    M.lift(x + (_: Int))(get)

  def kcomp[F[_], A, B, C](f: A => F[B], g: B => F[C])(implicit F: Bind[F]): A => F[C] =
    (F.bind(_: F[B])(g)) compose f

  def addNBad[M[_[_], _]](n: Int)(implicit M: MonadFree[M, G[Int]#O]): It[M]#f[Int, Int] =
    Stream.fill(n)(addGet[M]_)
      .foldLeft(M.point(_: Int))(kcomp[({type λ[α] = It[M]#f[Int, α]})#λ, Int, Int, Int](_, _))
      .apply(0)

  @tailrec
  def feedAll[M[_[_], _], A, B](fa: M[G[A]#O, B], as: List[A])(implicit M: MonadFree[M, G[A]#O]): Maybe[B] =
    M.resume(fa) match {
      case \/-(b) => Maybe.just(b)
      case -\/(g) => as match {
        case Nil    => Maybe.empty
        case h :: t => feedAll(g.f(h), t)
      }
    }

  type GInt[A] = Get[Int, A]

  def testQuadratic[M[_[_], _]](n: Int)(implicit M: MonadFree[M, GInt]) =
    feedAll(addNBad[M](n), (1 to n).toList)

  val count = Gen.range("count")(1000, 6000, 1000)

  performance of "TSeq Quadratic" in {
    using(count) in { s =>
      testQuadratic[Free](s)
    }
  }

  performance of "Scalaz Quadratic" in {
    using(count) in { s =>
      testQuadratic[ZFree](s)
    }
  }

}
