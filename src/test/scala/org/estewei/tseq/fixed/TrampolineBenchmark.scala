package org.estewei.tseq.fixed

import scala.Int
import scala.Stream
import org.scalameter.api._
import scalaz.{Free => FreeZ, Trampoline => TrampolineZ, _}
import scalaz.iteratee._

object TrampolineBenchmark extends PerformanceTest {

  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  def ackermannTA(m: Int, n: Int): Trampoline[Int] = {
    import Trampoline._
    if (m <= 0)
      delay(n + 1)
    else if (n <= 0)
      suspend(ackermannTA(m - 1, 1))
    else for {
      a <- suspend(ackermannTA(m, n - 1))
      b <- suspend(ackermannTA(m - 1, a))
    } yield b
  }

  def ackermannZ(m: Int, n: Int): FreeZ.Trampoline[Int] = {
    import TrampolineZ._
    if (m <= 0)
      delay(n + 1)
    else if (n <= 0)
      suspend(ackermannZ(m - 1, 1))
    else for {
      a <- suspend(ackermannZ(m, n - 1))
      b <- suspend(ackermannZ(m - 1, a))
    } yield b
  }

  val m = 3
  val ns = Gen.range("n")(1, 7, 1)

/*
[info] ::Benchmark TSeq Trampoline::
[info] cores: 2
[info] hostname: vera.local
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1): 0.078
[info] Parameters(n -> 2): 0.42
[info] Parameters(n -> 3): 2.17
[info] Parameters(n -> 4): 10.215
[info] Parameters(n -> 5): 42.867
[info] Parameters(n -> 6): 177.439
[info] Parameters(n -> 7): 739.643

[info] ::Benchmark Scalaz Trampoline::
[info] cores: 2
[info] hostname: vera.local
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1): 0.016
[info] Parameters(n -> 2): 0.078
[info] Parameters(n -> 3): 0.345
[info] Parameters(n -> 4): 1.693
[info] Parameters(n -> 5): 8.197
[info] Parameters(n -> 6): 33.485
[info] Parameters(n -> 7): 138.178
*/

/*
  performance of "TSeq Trampoline: raw" in {
    using(ns) in { n =>
      ackermannTA(m, n).run
    }
  }

  performance of "Scalaz Trampoline: raw" in {
    using(ns) in { n =>
      ackermannZ(m, n).run
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
