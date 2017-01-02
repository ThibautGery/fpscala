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
}
