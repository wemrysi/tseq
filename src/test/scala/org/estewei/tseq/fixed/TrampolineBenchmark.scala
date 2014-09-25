package org.estewei.tseq.fixed

import scala.{Int, Stream, Function0}
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

  final case class Get[I, O](f: I => O)
  type G[I] = { type O[o] = Get[I, o] }
  implicit def getFunctor[I]: Functor[G[I]#O] =
    new Functor[G[I]#O] {
      def map[A, B](g: Get[I, A])(f: A => B): Get[I, B] =
        Get(f compose g.f)
    }

  type ItT[I, O] = Free[G[I]#O, O]
  type ItZ[I, O] = ZFree[G[I]#O, O]

  def getT[I]: ItT[I, I] = Free.liftF[G[I]#O, I](Get(i => i))
  def getZ[I]: ItZ[I, I] = ZFree.liftF[G[I]#O, I](Get(i => i))
}
