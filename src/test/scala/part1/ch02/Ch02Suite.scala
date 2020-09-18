package part1.ch02

import org.scalatest.FunSuite

class Ch02Suite extends FunSuite {

  test("fib") {
    assert(Ch02.fib(0) === 0)
    assert(Ch02.fib(1) === 1)
    assert(Ch02.fib(2) === 1)
    assert(Ch02.fib(3) === 2)
    assert(Ch02.fib(4) === 3)
    assert(Ch02.fib(5) === 5)
  }

  test("isSorted") {

    assert(Ch02.isSorted[Int](Array(1,2,3,4), (a, b) => { a > b } ) === true)
    assert(Ch02.isSorted[Int](Array(1,2,3,4), (a, b) => { a < b } ) === false)
    assert(Ch02.isSorted[Int](Array(2,1,3,4), (a, b) => { a < b } ) === false)
    assert(Ch02.isSorted[Int](Array(4,2,1,3), (a, b) => { a < b } ) === false)
  }
}