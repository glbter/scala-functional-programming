package recfun

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunctionSuite extends AnyFunSuite {
  import Main.function

  test("function: x = 2") {
    assert(function(2, 3, 4).isNaN)
  }

  test("function: case 1, x = 1") {
    assert(function(1, 3, 4) === 2)
  }

  test("function: case 1, x = -1.5") {
    assert(function(-1.5, 10, 2000) === 2)
  }

  test("function: case 2, x = 3, m = 0, n = 0") {
    assert(function(3, 0, 0) === 2)
  }

  test("function: case 2, x = 3, m = 1, n = 1") {
    assert(function(3, 1, 1) === 6)
  }

  test("function: case 2, x = 4, m = 2, n = 4") {
    assert(function(4, 2, 4) === 272)
  }

  test("function: case 2, x = 4, m = -1, n = -1") {
    assert(function(4, -1, -1) === 0.5)
  }

  test("function: case 2, x = 4, m = -1, n = 1") {
    assert(function(4, -1, 0) === 1.25)
  }

}
