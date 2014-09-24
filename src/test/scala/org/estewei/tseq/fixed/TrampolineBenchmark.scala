package org.estewei.tseq.fixed

import scala.{Int, Stream, Function0}
import org.scalameter.api._
import scalaz.{Free => FreeZ, Trampoline => _, _}
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

  val m = 3
  val ns = Gen.range("n")(1, 7, 1)

/*

All benchmarks done on my ca. 2009 2.5GHz Core2Duo MacBook Pro

ackermann function with m = 3 and n varying from 1-7.

[info] ::Benchmark TSeq Trampoline: raw::
[info] cores: 2
[info] hostname: vera.local
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1): 0.081
[info] Parameters(n -> 2): 0.406
[info] Parameters(n -> 3): 2.018
[info] Parameters(n -> 4): 9.901
[info] Parameters(n -> 5): 41.303
[info] Parameters(n -> 6): 168.816
[info] Parameters(n -> 7): 705.076

[info] ::Benchmark Scalaz Trampoline: raw::
[info] cores: 2
[info] hostname: vera.local
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1): 0.018
[info] Parameters(n -> 2): 0.071
[info] Parameters(n -> 3): 0.305
[info] Parameters(n -> 4): 1.418
[info] Parameters(n -> 5): 6.568
[info] Parameters(n -> 6): 28.485
[info] Parameters(n -> 7): 116.739
*/

/*
  performance of "TSeq Trampoline: raw" in {
    using(ns) in { n =>
      ackermann[Free](m, n).run
    }
  }

  performance of "Scalaz Trampoline: raw" in {
    using(ns) in { n =>
      ackermann[FreeZ](m, n).run
    }
  }
*/

  // Iteratee Usage

  def iter[F[_]: Applicative] = Iteratee.fold[Int, Id.Id, Int](0){ case (a,v) => a + v }.up[F]
  def enum[F[_]: Monad](max: Int) = EnumeratorT.enumStream[Int, F](Stream.fill(max)(1))

  val size = Gen.range("size")(10000, 50000, 10000)

  performance of "TSeq Iteratee" in {
    using(size) in { s =>
      (iter[Trampoline] &= enum[Trampoline](s)).run.run
    }
  }

  performance of "Scalaz Iteratee" in {
    using(size) in { s =>
      (iter[FreeZ.Trampoline] &= enum[FreeZ.Trampoline](s)).run.run
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
  type ItZ[I, O] = FreeZ[G[I]#O, O]

  def getT[I]: ItT[I, I] = Free.liftF[G[I]#O, I](Get(i => i))
  def getZ[I]: ItZ[I, I] = FreeZ.liftF[G[I]#O, I](Get(i => i))
}
