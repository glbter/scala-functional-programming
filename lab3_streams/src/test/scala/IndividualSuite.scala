class IndividualSuite extends munit.FunSuite{
  val l: List[Int] = List.range(-255, 255 +1, 1)

  val l2: List[Int] = List.range(-255, 2, 1).appendedAll(List.range(3, 255 + 1, 1))
  val r: Seq[Int] = -255 to 255

  val toList: Seq[Int] => List[Double] = IndividualFunc.powerSumList(1, 1)

  test("sequence test") {
    assertEquals(toList(r).length, 510)
  }

  test("filter") {
    val filtered = IndividualFunc.filter(toList(r), (x: Double) => x > 3)

    assertEquals(filtered.head, 6.0)
    assertEquals(filtered.last, 510.0)
  }

  test("map") {
    val mapped = IndividualFunc.map(toList(r), (x: Double) => x.toInt)

    assertEquals(mapped.head, 2)
    assertEquals(mapped.last, 510)
  }

  test("fold") {
    val folded = IndividualFunc.fold(toList(r).filter(p => p == 2),
      (x: Double, y: Double) => x + y)

    assertEquals(folded, 514.0)
  }

  test("reduce") {
    val reduced = IndividualFunc.reduce(0.0,
      toList(r).filter(p => p == 2),
      (x: Double, y: Double) => x + y)

    assertEquals(reduced, 514.0)
  }

  test("for each") {
    var acc = 0.0
    IndividualFunc.foreach(
      toList(r).filter(p => p == 2),
      (x: Double) => acc += x)

    assertEquals(acc, 514.0)
  }
}
