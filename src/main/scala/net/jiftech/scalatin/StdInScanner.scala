package net.jiftech.scalatin

/* copy/paste from here... */
import scala.io.StdIn

case class StdInScanner[A](scan: StdIn.type => A) {
  // transform read result by "f"
  def map[B](f: A => B): StdInScanner[B] =
    StdInScanner(in => f(scan(in)))

  // run this to read A, then obtain new scanner from A using "f", then run the new scanner
  def flatMap[B](f: A => StdInScanner[B]): StdInScanner[B] =
    StdInScanner(in => f(scan(in)).scan(in))

  // dummy implementation. this enables pattern matching in for-comprehensions 
  def withFilter(f: A => Boolean): StdInScanner[A] = this

  // run entire scanner and get result
  def run: A = scan(StdIn)
}

object StdInScanner {
  /* pure(read nothing) scanners */
  // read nothing, and return specified "a"
  def of[A](a: A): StdInScanner[A] = StdInScanner(_ => a)
  // read nothing, and return ()
  def unit: StdInScanner[Unit] = StdInScanner.of(())

  /* read single line -> produce single value */
  // read single line
  def readLine: StdInScanner[String] = StdInScanner(s => s.readLine().trim)
  
  // read single line as Int
  def readInt: StdInScanner[Int] = StdInScanner(s => s.readInt())

  // read single line as Long
  def readLong: StdInScanner[Long] = StdInScanner(s => s.readLong())

  // read single line as BigInt
  def readBigInt: StdInScanner[BigInt] = readLine.map(BigInt(_))

  // read single line as a pair of values
  def readPair[A, B](fa: String => A, fb: String => B): StdInScanner[(A, B)] =
    readTokens.map(v => (fa(v(0)), fb(v(1))))

  def readStringPair: StdInScanner[(String, String)] =
    readPair(identity, identity)

  def readIntPair: StdInScanner[(Int, Int)] =
    readPair(_.toInt, _.toInt)

  def readLongPair: StdInScanner[(Long, Long)] =
    readPair(_.toLong, _.toLong)

  // read single line as a triple of values
  def readTriple[A, B, C](fa: String => A, fb: String => B, fc: String => C): StdInScanner[(A, B, C)] =
    readTokens.map(v => (fa(v(0)), fb(v(1)), fc(v(2))))

  def readStringTriple: StdInScanner[(String, String, String)] =
    readTriple(identity, identity, identity)

  def readIntTriple: StdInScanner[(Int, Int, Int)] =
    readTriple(_.toInt, _.toInt, _.toInt)

  def readLongTriple: StdInScanner[(Long, Long, Long)] =
    readTriple(_.toLong, _.toLong, _.toLong)

  /* read single line -> produce multiple values */
  // read tokens from a line, then map them with "f"
  def readTokensWithMap[A](f: String => A): StdInScanner[Vector[A]] =
    readLine.map(_.split(" ").toVector.map(f))

  def readTokens: StdInScanner[Vector[String]] =
    readTokensWithMap(identity)

  def readIntTokens: StdInScanner[Vector[Int]] =
    readTokensWithMap(_.toInt)

  def readLongTokens: StdInScanner[Vector[Long]] =
    readTokensWithMap(_.toLong)

  def readBigIntTokens: StdInScanner[Vector[BigInt]] =
    readTokensWithMap(BigInt(_))

  /* read multiple lines -> produce one value per line */
  def readLines(n: Int): StdInScanner[Vector[String]] =
    StdInScanner(s => Vector.fill(n)(s.readLine().trim))

  def readLinesWithMap[A](n: Int, f: String => A): StdInScanner[Vector[A]] =
    StdInScanner(s => Vector.fill(n)(f(s.readLine())))

  def readInts(n: Int): StdInScanner[Vector[Int]] =
    StdInScanner(s => Vector.fill(n)(s.readInt()))

  def readLongs(n: Int): StdInScanner[Vector[Long]] =
    StdInScanner(s => Vector.fill(n)(s.readLong()))

  def readBigInts(n: Int): StdInScanner[Vector[BigInt]] =
    StdInScanner(s => Vector.fill(n)(BigInt(s.readLine())))

  def readPairs[A, B](n: Int)(fa: String => A, fb: String => B): StdInScanner[Vector[(A, B)]] =
    readLines(n).map(_.map{ line =>
      val t1 :: t2 :: _ = line.split(" ").toList
      (fa(t1), fb(t2))
    })
  def readIntPairs(n: Int): StdInScanner[Vector[(Int, Int)]] =
    readPairs(n)(_.toInt, _.toInt)

  def readTriples[A, B, C](n: Int)(fa: String => A, fb: String => B, fc: String => C): StdInScanner[Vector[(A, B, C)]] =
    readLines(n).map(_.map{ line =>
      val t1 :: t2 :: t3 :: _ = line.split(" ").toList
      (fa(t1), fb(t2), fc(t3))
    })

  def readIntTriples(n: Int): StdInScanner[Vector[(Int, Int, Int)]] =
    readTriples(n)(_.toInt, _.toInt, _.toInt)

  /* read multiple lines -> produce multiple value per line */
  type Matrix[A] = Vector[Vector[A]]

  def readMatrixWithMap[A](nRows: Int, f: String => A): StdInScanner[Matrix[A]] =
    readLines(nRows).map{ lns =>
      lns.map(ln => ln.split(" ").toVector.map(tk => f(tk)))
    }
  def readMatrix(nRows: Int): StdInScanner[Matrix[String]] =
    readMatrixWithMap(nRows, identity)

  def readIntMatrix(nRows: Int): StdInScanner[Matrix[Int]] =
    readMatrixWithMap(nRows, _.toInt)

  def readLongMatrix(nRows: Int): StdInScanner[Matrix[Long]] =
    readMatrixWithMap(nRows, _.toLong)

  def readBigIntMatrix(nRows: Int): StdInScanner[Matrix[BigInt]] =
    readMatrixWithMap(nRows, BigInt(_))
}
/* ... to here */

// extension methods of StdInScanner, enables to run scanner on stub input. useful for testing.
object StdInScannerSyntaxForTesting {
  implicit class StdInScannerOps[A](private val sc: StdInScanner[A]) extends AnyVal {
    import java.io.StringReader
    import java.nio.file.{Files, Paths}

    // run entire scanner on the text file
    def runOnFile(filename: String): A =
    Console.withIn(Files.newBufferedReader(Paths.get(filename)))(sc.scan(StdIn))

    // run entire scanner on the String
    def runOnString(str: String): A =
    Console.withIn(new StringReader(str))(sc.scan(StdIn))
  }
}