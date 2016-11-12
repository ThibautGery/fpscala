import org.specs2.mutable.Specification

class TestSpecs extends Specification {
  "this is my specification" should {
    "have one example" in {
      1 must_== 1
    }
    "and another one" in {
      2 must_== 2
    }
  }
}