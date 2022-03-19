package net.jiftech.scalatin

import org.scalatest.flatspec.*

import StdInScannerTestingExt.*

class StdInScannerSpec extends AnyFlatSpec {
  "StdInScanner.pure" should "emit applied value" in {
    val scanner = StdInScanner.pure(true)
    val res = scanner.run
    assert(res == true)
  }

  "readLine" should "read a line" in {
    val res = StdInScanner.readLine.runOnString("test")
    assert(res == "test")
  }

  "readLineAs[Int]" should "read an int" in {
    val res = StdInScanner.readLineAs[Int].runOnString("42")
    assert(res == 42)
  }

  "readLineAs[Boolean]" should "read an boolean" in {
    val res = StdInScanner.readLineAs[Boolean].runOnString("true")
    assert(res == true)
  }
  it should "emulate StdIn.readBoolean's behaviour" in {
    val cases = Map(
      "true" -> true,
      "t" -> true,
      "yes" -> true,
      "y" -> true,
      "false" -> false,
      "no" -> false,
      "foobar" -> false
    )
    for (in, exp) <- cases do {
      val res = StdInScanner.readLineAs[Boolean].runOnString(in)
      assert(res == exp)
    }
  }

  "readLineAs[Long]" should "read an Long int" in {
    val res = StdInScanner.readLineAs[Long].runOnString("12345678901")
    assert(res == 12345678901L)
  }

  "readLineAs[BigInt]" should "read an BigInt" in {
    val res =
      StdInScanner.readLineAs[BigInt].runOnString("12345678901234567890")
    assert(res == BigInt(1234567890123456789L) * BigInt(10))
  }

  "readLineAs[Double]" should "read an Double" in {
    val res = StdInScanner.readLineAs[Double].runOnString("12345.6789")
    assert(res === 12345.6789)
  }

  "readLineAs[BigDecimal]" should "read an BigDecimal" in {
    val res =
      StdInScanner.readLineAs[BigDecimal].runOnString("1234567890.0987654321")
    assert(res == BigDecimal("1234567890.0987654321"))
  }

  "readLineAs[(Int, String)]" should "read an Int-String pair" in {
    val res = StdInScanner.readLineAs[(Int, String)].runOnString("100 points")
    assert(res == (100, "points"))
  }

  "readLineAs[(Int, String, Boolean)]" should "read an Int-String-Boolean triple" in {
    val res = StdInScanner
      .readLineAs[(Int, String, Boolean)]
      .runOnString("100 points yes")
    assert(res == (100, "points", true))
  }

  "readLineAs[Vector[String]]" should "read Vector of tokens (String) from a line" in {
    val res = StdInScanner.readLineAs[Vector[String]].runOnString("I am a pen.")
    assert(res == Vector("I", "am", "a", "pen."))
  }

  "readLineAs[List[String]]" should "read List of tokens (String)" in {
    val res = StdInScanner.readLineAs[List[String]].runOnString("I am a pen.")
    assert(res == List("I", "am", "a", "pen."))
  }

  "readLineAs[Set[String]]" should "read Set of tokens (String)" in {
    val res =
      StdInScanner.readLineAs[Set[String]].runOnString("this is as it is")
    assert(res == Set("this", "is", "as", "it"))
  }

  "readLineAs[Queue[String]]" should "read tokens (String) and construct Queue from them" in {
    import scala.collection.mutable.Queue
    val res =
      StdInScanner.readLineAs[Queue[String]].runOnString("first in first out")

    assert(
      (res.dequeueAll(_ => true) zip List("first", "in", "first", "out"))
        .forall(_ == _)
    )
  }

  "readLineAs[Vector[Int]]" should "read Vector of int tokens from a line" in {
    val res =
      StdInScanner.readLineAs[Vector[Int]].runOnString("2 3 5 7 11 13 17 19")
    assert(res == Vector(2, 3, 5, 7, 11, 13, 17, 19))
  }

  "readLinesAs[Vector[String]]" should "read N lines as Vector of Strings" in {
    val in = "hoge\nfuga\npoyo\npiyo"
    val n = 4
    val res = StdInScanner.readLinesAs[Vector[String]](n).runOnString(in)

    assert(res.length == n)
    assert(res == Vector("hoge", "fuga", "poyo", "piyo"))
  }

  "readLinesAs[Vector[Int]]" should "read N lines as Vector of Ints" in {
    val in = "1\n1\n2\n3\n5\n8\n13\n21"
    val n = 8
    val res = StdInScanner.readLinesAs[Vector[Int]](n).runOnString(in)

    assert(res.length == n)
    assert(res == Vector(1, 1, 2, 3, 5, 8, 13, 21))
  }

  "readLinesAs[Vector[(String, Int)]]" should "read N lines as Vector of pairs" in {
    val in = "a 1\nb 2\nc 3\nd 4"
    val n = 4
    val res = StdInScanner.readLinesAs[Vector[(String, Int)]](n).runOnString(in)

    assert(res.length == n)
    assert(
      res == Vector(
        ("a", 1),
        ("b", 2),
        ("c", 3),
        ("d", 4)
      )
    )
  }

  "readLinesAs[Array[String]]" should "read N lines as Array of strings" in {
    val in = "hoge\nfuga\npoyo\npiyo"
    val n = 4
    val res = StdInScanner.readLinesAs[Array[String]](n).runOnString(in)

    assert(res.length == n)
    assert(res === Array("hoge", "fuga", "poyo", "piyo"))
  }

  "readLinesAs[SortedSet[Int]]" should "read N lines as SortedSet of ints" in {
    import scala.collection.immutable.SortedSet

    val in = "3\n4\n1\n2\n4\n3"
    val n = 6
    val res = StdInScanner.readLinesAs[SortedSet[Int]](n).runOnString(in)

    assert(res == SortedSet(3, 4, 1, 2))
    assert((res.iterator zip List(1, 2, 3, 4)).forall(_ == _))
  }

  "readLinesAs[PriorityQueue[Int]]" should "read N lines as PriorityQueue of ints" in {
    import scala.collection.mutable.PriorityQueue

    val in = "3\n4\n1\n2"
    val n = 4
    val res =
      StdInScanner.readLinesAs[PriorityQueue[Int]](n).runOnString(in)

    // dequeue() / dequeueAll() returns element that has highest priority first.
    assert((res.dequeueAll zip List(4, 3, 2, 1)).forall(_ == _))
  }

  "readLinesAs[Map[String, Int]]" should "read N lines as map contains key-value pairs parsed from each line" in {
    val in = "a 1\nb 2\nc 3\nd 4"
    val n = 4
    val res = StdInScanner.readLinesAs[Map[String, Int]](n).runOnString(in)

    assert(
      res == Map(
        "a" -> 1,
        "b" -> 2,
        "c" -> 3,
        "d" -> 4
      )
    )
  }

  "readLinesAs[SortedMap[String, Int]]" should "read N lines as sorted map contains key-value pairs parsed from each line" in {
    import scala.collection.immutable.SortedMap

    val in = "c 1\nd 2\na 3\nb 4"
    val n = 4
    val res = StdInScanner
      .readLinesAs[SortedMap[String, Int]](n)
      .runOnString(in)

    assert(
      res == SortedMap(
        "c" -> 1,
        "d" -> 2,
        "a" -> 3,
        "b" -> 4
      )
    )

    assert((res.keysIterator zip List("a", "b", "c", "d")).forall(_ == _))
  }

  "readMatrixOf[String]" should "read N lines as matrix" in {
    val in = "a b c\na r c\na g c"
    val rows = 3
    val res = StdInScanner.readMatrixOf[String](rows).runOnString(in)

    assert(res.length == rows)
    assert(
      res == Vector(
        Vector("a", "b", "c"),
        Vector("a", "r", "c"),
        Vector("a", "g", "c")
      )
    )
  }

  "readMatrixOf[Int]" should "read N lines as matrix of int" in {
    val in = "1 1 4\n5 1 4\n1 9 1 9\n8 1 0"
    val rows = 4
    val res = StdInScanner.readMatrixOf[Int](rows).runOnString(in)

    assert(res.length == rows)
    assert(
      res == Vector(
        Vector(1, 1, 4),
        Vector(5, 1, 4),
        Vector(1, 9, 1, 9),
        Vector(8, 1, 0)
      )
    )
  }

  "StdInScanner" should "be composable in for-comprehensions" in {
    import StdInScanner._
    import scala.collection.immutable.BitSet

    val in = """
    |2 3
    |100
    |200
    |A true
    |B true
    |C false
    |3 2 1 3 2 3
    """.trim.stripMargin

    val scanner = for {
      p <- pure(100)
      (m, n) <- readLineAs[(Int, Int)]
      v <- readLinesAs[Vector[Int]](m)
      m <- readLinesAs[Map[String, Boolean]](n)
      s <- readLineAs[BitSet]
    } yield (p, v, m, s)
    val (resP, resV, resM, resS) = scanner.runOnString(in)

    assert(resP == 100)
    assert(resV == Vector(100, 200))
    assert(resM == Map("A" -> true, "B" -> true, "C" -> false))
    assert(resS == BitSet(1, 2, 3))
  }
}
