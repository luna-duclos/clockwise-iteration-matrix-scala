import org.scalatest.funsuite.AnyFunSuite

import scala.runtime.stdLibPatches.Predef.assert

class ClockWiseMatrixTest extends AnyFunSuite {
  test("ClockWiseMatrixTest.example") {
    var example = Array[Array[Int]](
      Array[Int](2, 3, 4, 8),
      Array[Int](5, 7, 9, 12),
      Array[Int](1, 0, 6, 10),
    )

    assert(clockwiseMatrix(example) == "2, 3, 4, 8, 12, 10, 6, 0, 1, 5, 7, 9")
  }
}