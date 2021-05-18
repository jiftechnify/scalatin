package net.jiftech.scalatin

import org.scalatest.flatspec.*
import org.scalactic.TolerantNumerics
import org.scalactic.TripleEquals.*
import StdInScannerSyntaxForTesting.*

class StdInScannerSpec extends AnyFlatSpec {
  "StdInScanner.of" should "emit applied value" in {
    val scanner = StdInScanner.of(true)
    val res = scanner.run
    assert(res == true)
  }

  "readLine" should "read a line" in {
    val res = StdInScanner.readLine.runOnString("test")
    assert(res == "test")
  }

  "read[Int]" should "read an int" in {
    val res = StdInScanner.read[Int].runOnString("42")
    assert(res == 42)
  }

  "read[Long]" should "read an Long int" in {
    val res= StdInScanner.read[Long].runOnString("12345678901")
    assert(res == 12345678901L)
  }

  "read[BigInt]" should "read an BigInt" in {
    val res = StdInScanner.read[BigInt].runOnString("12345678901234567890")
    assert(res == BigInt(1234567890123456789L) * BigInt(10))
  }

  "read[Double]" should "read an Double" in {
    val res = StdInScanner.read[Double].runOnString("12345.6789")
    assert(res === 12345.6789)
  }

  // TODO:
  // "read[BigDecimal]" should "read an BigDecimal" in {
  //   val res = StdInScanner.read[BigDecimal].runOnString("")
  // }

  "readPair" should "read 2 values as pair from a line" in {
    val res = StdInScanner.readPairWithMap(_.toLowerCase, _.length).runOnString("StdInScanner hogefuga")
    assert(res == ("stdinscanner", 8))
  }

  "readIntPair" should "read 2 ints as pair from a line" in {
    val res = StdInScanner.readPairOfSameType[Int].runOnString("114 514")
    assert(res == (114, 514))
  }

  "readTriple" should "read 3 values as triple from a line" in {
    val res = StdInScanner.readTripleWithMap(_.toLowerCase, _.length, _.toBoolean)
      .runOnString("readTriple piyopi false")
    assert(res == ("readtriple", 6, false))
  }

  "readIntTriple" should "read 3 ints as pair from a line" in {
    val res = StdInScanner.readTripleOfSameType[Int].runOnString("11 45 14")
    assert(res == (11, 45, 14))
  }

  "readTokens[Vector]" should "read Vector of tokens from a line" in {
    val res: Vector[String] = StdInScanner.readTokens[Vector].runOnString("I am a pen.")
    assert(res == Vector("I", "am", "a", "pen."))
  }

  "readTokens[List]" should "read List of tokens" in {
    val res: List[String] = StdInScanner.readTokens[List].runOnString("I am a pen.")
    assert(res == List("I", "am", "a", "pen."))
  }

  "readTokens[Set]" should "read Set of tokens" in {
    val res: Set[String] = StdInScanner.readTokens[Set].runOnString("this is as it is")
    assert(res == Set("this", "is", "as", "it"))
  }

  "readTokens[Queue]" should "read tokens and construct queue from them" in {
    import scala.collection.mutable.Queue
    val res: Queue[String] = StdInScanner.readTokens[Queue].runOnString("first in first out")
    assert(res.dequeue == "first")
    assert(res.dequeue == "in")
    assert(res.dequeue == "first")
    assert(res.dequeue == "out")
  }

  // TODO: test on all collection types

  "readIntTokens" should "read Vector of int tokens from a line" in {
    val res = StdInScanner.readTokensOf[Vector, Int].runOnString("2 3 5 7 11 13 17 19")
    assert(res == Vector(2, 3, 5, 7, 11, 13, 17, 19))
  }

  "readLines" should "read N lines as Vector" in {
    val in = "hoge\nfuga\npoyo\npiyo"
    val n = 4
    val res = StdInScanner.readLines[Vector](n).runOnString(in)

    assert(res.length == n)
    assert(res == Vector("hoge", "fuga", "poyo", "piyo"))
  }

  "readInts" should "read N lines of ints as Vector" in {
    val in = "1\n1\n2\n3\n5\n8\n13\n21"
    val n = 8
    val res = StdInScanner.readLinesOf[Vector, Int](n).runOnString(in)

    assert(res.length == n)
    assert(res == Vector(1, 1, 2, 3, 5, 8, 13, 21))
  }

  "readPairs" should "read N lines of pair as Vector" in {
    val in = "a 1\nb 2\nc 3\nd 4"
    val n = 4
    val res = StdInScanner.readPairsWithMap[Vector, String, Int](n)(_.toUpperCase, _.toInt).runOnString(in)

    assert(res.length == n)
    assert(res == Vector(
      ("A", 1),
      ("B", 2),
      ("C", 3),
      ("D", 4)
    ))
  }

  "readMapOf" should "read N lines as map contains key-value pairs parsed from each line" in {
    val in = "a 1\nb 2\nc 3\nd 4"
    val n = 4
    val res = StdInScanner.readMapOf[String, Int](n).runOnString(in)

    assert(res == Map(
      "a" -> 1,
      "b" -> 2,
      "c" -> 3,
      "d" -> 4
    ))
  }

  "readSortedMapOf" should "read N lines as sorted map contains key-value pairs parsed from each line" in {
    import scala.collection.immutable.SortedMap

    val in = "c 3\nd 4\na 1\nb 2"
    val n = 4
    val res = StdInScanner.readSortedMapOf[String, Int](n).runOnString(in)

    assert(res == SortedMap(
      "c" -> 3,
      "d" -> 4,
      "a" -> 1,
      "b" -> 2
    ))
    assert((res.keysIterator zip List("a", "b", "c", "d")).forall(_ == _))
  }

  "readMatrix" should "read N lines as matrix" in {
    val in = "a b c\na r c\na g c"
    val rows = 3
    val res = StdInScanner.readMatrix(rows).runOnString(in)

    assert(res.length == rows)
    assert(res == Vector(
      Vector("a", "b", "c"),
      Vector("a", "r", "c"),
      Vector("a", "g", "c")
    ))
  }

  "readIntMatrix" should "read N lines as matrix of int" in {
    val in = "1 1 4\n5 1 4\n1 9 1 9\n8 1 0"
    val rows = 4
    val res = StdInScanner.readMatrixOf[Int](rows).runOnString(in)

    assert(res.length == rows)
    assert(res == Vector(
      Vector(1, 1, 4),
      Vector(5, 1, 4),
      Vector(1, 9, 1, 9),
      Vector(8, 1, 0)
    ))
  }

  "StdInScanner" should "be composable in for-comprehensions" in {
    import StdInScanner._

    val in = """
    |2 3
    |100
    |200
    |1 2
    |3 4
    |5 6
    """.trim.stripMargin

    val scanner = for {
      c      <- StdInScanner.of(100)
      (m, n) <- readPairOfSameType[Int]
      a      <- readLinesOf[Vector, Int](m)
      b      <- readPairsOfSameType[Vector, Int](n)
    } yield (a, b, c)
    val (resA, resB, resC) = scanner.runOnString(in)

    assert(resA == Vector(100, 200))
    assert(resB == Vector((1, 2), (3, 4), (5, 6)))
    assert(resC == 100)
  }
}
