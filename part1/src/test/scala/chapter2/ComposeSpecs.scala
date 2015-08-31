package chapter2

class ComposeSpecs extends org.specs2.mutable.Specification {


  "String.length Int*2" >> {
    "works" >> {
      Compose.compose((b: Int) => b*2, (a: String) => a.length)("double length") must_== 26
    }
  }
}