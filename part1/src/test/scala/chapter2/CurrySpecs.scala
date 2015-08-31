package chapter2

class CurrySpecs extends org.specs2.mutable.Specification {


  "Currying" >> {
    "works" >> {
      Curry.curry((a: Int, b: String) => a + b.length)(1)("22") must_== 3
    }
  }
}