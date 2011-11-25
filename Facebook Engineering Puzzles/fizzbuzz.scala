import org.scalatest.FunSuite

object fizbz {
  //the actual fizzbuzz printout
  var arr = Array[Any]()
  (1 to 100).foreach(arr :+= this.fizzbuzz(_))
  arr.foreach(println(_))

  def fizzbuzz(i:Int) = {
    var fb:Any = null
    if (i % 3 == 0 && i % 5 == 0) {fb = "FizzBuzz"}
    else if (i % 3 == 0 && i % 5 != 0) {fb = "Fizz"}
    else if (i % 5 == 0 && i % 3 != 0) {fb = "Buzz"}
    else {fb = i}
    fb
  }
}


class fizzbuzz extends FunSuite {
  test("15 = FizzBuzz") {
    assert(fizbz.fizzbuzz(15) === "FizzBuzz")
  }
  test("75 = FizzBuzz") {
    assert(fizbz.fizzbuzz(75) === "FizzBuzz")
  }
  test("20 = Buzz") {
    assert(fizbz.fizzbuzz(20) === "Buzz")
  }
  test("85 = Buzz") {
    assert(fizbz.fizzbuzz(85) === "Buzz")
  }
  test("78 = Fizz") {
    assert(fizbz.fizzbuzz(78) === "Fizz")
  }
  test("42 = Fizz") {
    assert(fizbz.fizzbuzz(42) === "Fizz")
  }
  test("17 = 17") {
    assert(fizbz.fizzbuzz(17) === 17)
  }
  test("73 = 73") {
    assert(fizbz.fizzbuzz(73) === 73)
  }
  test("98 = 98") {
    assert(fizbz.fizzbuzz(98) === 98)
  }
}
