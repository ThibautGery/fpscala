package fpinscala.parallel

import java.util.concurrent.ExecutorService

import fpinscala.parallel.Par.Par
import org.specs2.mutable.Specification

import scala.concurrent.ExecutionContext


class ParTest extends Specification {

  val exec = ExecutionContext.fromExecutorService(null: ExecutorService)
  
  def sum(ints: Seq[Int]): Par[Int] = if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }

  "the sum function" should {
    "return the sum" in {
      val result = sum(List(1, 2, 3))

      result(exec).get must_== 6
    }
  }

  "asyncF" should {
    def sum2(ints: Seq[Int]): Int = ints.sum

    "ouput a Par object" in {
      val l = List(1, 2, 3)
      val expect = sum(l)
      val actual = Par.asyncF(sum2)(l)

      expect(exec).get must_== actual(exec).get
    }
  }

  "run" should {
    "compute the Par function" in {
      Par.run(exec)(Par.unit(1)).get() must_== 1
    }
  }

  "sortPar" should {
    "sort the list" in {
      val l = Par.unit(List(1, 3, 2))

      val sortedPar = Par.sortPar(l)

      sortedPar(exec).get must_== List(1, 2, 3)
    }
  }

  "sequence" should {
    "a list of par as a par of list" in {
      val l = List(Par.unit(1), Par.unit(2))
      Par.sequence(l)(exec).get must_== List(1, 2)
    }
  }

  "parMap" should {
    "map over the item" in {
      val l = List(1, 2)
      Par.parMap(l)(_ + 1)(exec).get must_== List(2, 3)
    }
  }

  "parFilter" should {
    "filter the collection" in {
      val l = List(1, 2)
      Par.parFilter(l)(_ % 2 == 0)(exec).get must_== List(2)
    }
  }
}
