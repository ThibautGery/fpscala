package chapter2

class UncurrySpecs extends org.specs2.mutable.Specification {

  "Uncurrying" >> {
    "works" >> {
      Uncurry.uncurry((a: Int) => (b: String) => a + b.length)(1,"22") must_== 3
    }
  }
}