package net.jiftech.scalatin

import org.scalatest.flatspec.*
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

  "readInt" should "read an int" in {
    val res = StdInScanner.readInt.runOnString("42")
    assert(res == 42)
  }

  "readLong" should "read an Long int" in {
    val res= StdInScanner.readLong.runOnString("12345678901")
    assert(res == 12345678901L)
  }

  "readBigInt" should "read an BigInt" in {
    val res = StdInScanner.readBigInt.runOnString("12345678901234567890")
    assert(res == BigInt(1234567890123456789L) * BigInt(10))
  }

  "readPair" should "read 2 values as pair from a line" in {
    val res = StdInScanner.readPair(_.toLowerCase, _.length).runOnString("StdInScanner hogefuga")
    assert(res == ("stdinscanner", 8))
  }

  "readIntPair" should "read 2 ints as pair from a line" in {
    val res = StdInScanner.readIntPair.runOnString("114 514")
    assert(res == (114, 514))
  }

  "readTriple" should "read 3 values as triple from a line" in {
    val res = StdInScanner.readTriple(_.toLowerCase, _.length, _.toBoolean)
      .runOnString("readTriple piyopi false")
    assert(res == ("readtriple", 6, false))
  }

  "readIntTriple" should "read 3 ints as pair from a line" in {
    val res = StdInScanner.readIntTriple.runOnString("11 45 14")
    assert(res == (11, 45, 14))
  }

  "readTokens" should "read Vector of tokens from a line" in {
    val res = StdInScanner.readTokens.runOnString("I am a pen.")
    assert(res == Vector("I", "am", "a", "pen."))
  }

  "readIntTokens" should "read Vector of int tokens from a line" in {
    val res = StdInScanner.readIntTokens.runOnString("2 3 5 7 11 13 17 19")
    assert(res == Vector(2, 3, 5, 7, 11, 13, 17, 19))
  }

  "readLines" should "read N lines as Vector" in {
    val in = "hoge\nfuga\npoyo\npiyo"
    val n = 4
    val res = StdInScanner.readLines(n).runOnString(in)

    assert(res.length == n)
    assert(res == Vector("hoge", "fuga", "poyo", "piyo"))
  }

  "readInts" should "read N lines of ints as Vector" in {
    val in = "1\n1\n2\n3\n5\n8\n13\n21"
    val n = 8
    val res= StdInScanner.readInts(n).runOnString(in)

    assert(res.length == n)
    assert(res == Vector(1, 1, 2, 3, 5, 8, 13, 21))
  }

  "readPairs" should "read N lines of pair as Vector" in {
    val in = "a 1\nb 2\nc 3\nd 4"
    val n = 4
    val res = StdInScanner.readPairs(n)(_.toUpperCase, _.toInt).runOnString(in)

    assert(res.length == n)
    assert(res == Vector(
      ("A", 1),
      ("B", 2),
      ("C", 3),
      ("D", 4)
    ))
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
    val res = StdInScanner.readIntMatrix(rows).runOnString(in)

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
      (m, n) <- readIntPair
      a      <- readInts(m)
      b      <- readIntPairs(n)
    } yield (a, b, c)
    val (resA, resB, resC) = scanner.runOnString(in)

    assert(resA == Vector(100, 200))
    assert(resB == Vector((1, 2), (3, 4), (5, 6)))
    assert(resC == 100)
  }
}
