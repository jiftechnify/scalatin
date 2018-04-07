package net.jiftech.scalatin

import java.io.StringReader
import java.nio.file.{Files, Paths}

import scala.io.StdIn
import scala.language.higherKinds
import scala.reflect.ClassTag

case class StdInScanner[A](scan: StdIn.type => A) {
  // transform read result by "f"
  def map[B](f: A => B): StdInScanner[B] =
    StdInScanner(in => f(scan(in)))

  // run this to read A, then obtain new scanner from A using "f", then run the new scanner
  def flatMap[B](f: A => StdInScanner[B]): StdInScanner[B] =
    StdInScanner(in => f(scan(in)).scan(in))

  // run entire scanner and get result
  def run: A = scan(StdIn)

  // run entire scanner on the text file
  def runOnFile(filename: String): A =
    Console.withIn(Files.newBufferedReader(Paths.get(filename)))(this.scan(StdIn))

  // run entire scanner on the String
  def runOnString(str: String): A =
    Console.withIn(new StringReader(str))(this.scan(StdIn))
}

object StdInScanner {
  /* pure(read nothing) scanners */
  // read nothing, and return specified "a"
  def apply[A](a: A): StdInScanner[A] = StdInScanner(_ => a)
  // read nothing, and return ()
  def unit: StdInScanner[Unit] = StdInScanner(())

  /* read single line -> produce single value */
  // read a line
  def readALine: StdInScanner[String] = StdInScanner(s => s.readLine().trim)
  // read a line as Int
  def readAnInt: StdInScanner[Int] = StdInScanner(s => s.readInt())
  // read a line as BigInt
  def readABigInt: StdInScanner[BigInt] = readALine.map(BigInt(_))

  def readAPair[A, B](fa: String => A, fb: String => B): StdInScanner[(A, B)] =
    readTokens.map(v => (fa(v(0)), fb(v(1))))

  def readIntPair: StdInScanner[(Int, Int)] =
    readAPair(_.toInt, _.toInt)

  def readATriple[A, B, C](fa: String => A, fb: String => B, fc: String => C): StdInScanner[(A, B, C)] =
    readTokens.map(v => (fa(v(0)), fb(v(1)), fc(v(2))))

  def readIntTriple: StdInScanner[(Int, Int, Int)] =
    readATriple(_.toInt, _.toInt, _.toInt)

  def readTwoPairs[A, B](fa: String => A, fb: String => B): StdInScanner[((A, B), (A, B))] =
    readTokens.map(v => ((fa(v(0)), fb(v(1))), (fa(v(2)), fb(v(3)))))

  def readTwoIntPairs: StdInScanner[((Int, Int), (Int, Int))] =
    readTwoPairs(_.toInt, _.toInt)

  /* read single line -> produce multiple values */
  // read tokens from a line, then map them with "f"
  def readTokensWithMap[A](f: String => A): StdInScanner[Vector[A]] =
    readALine.map(_.split(" ").toVector.map(f))

  def readTokens: StdInScanner[Vector[String]] =
    readTokensWithMap(identity)

  def readIntTokens: StdInScanner[Vector[Int]] =
    readTokensWithMap(_.toInt)

  def readBigIntTokens: StdInScanner[Vector[BigInt]] =
    readTokensWithMap(BigInt(_))

  /* read multiple lines -> produce one value per line */
  def readLines(n: Int): StdInScanner[Vector[String]] =
    StdInScanner(s => Vector.fill(n)(s.readLine().trim))

  def readLinesWithMap[A](n: Int, f: String => A): StdInScanner[Vector[A]] =
    StdInScanner(s => Vector.fill(n)(f(s.readLine())))

  def readInts(n: Int): StdInScanner[Vector[Int]] =
    StdInScanner(s => Vector.fill(n)(s.readInt()))

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
  // "2D Matrix of A" is Vector of "Vector of A"
  type Matrix[A] = Vector[Vector[A]]

  // read a table of "rows", then map each elements with "f"
  def readTableWithMap[A: ClassTag](rows: Int, f: String => A): StdInScanner[Matrix[A]] =
    readLines(rows).map{ lns =>
      lns.map(ln => ln.split(" ").toVector.map(tkn => f(tkn)))
    }
  def readTable(rows: Int): StdInScanner[Matrix[String]] =
    readTableWithMap(rows, identity)

  def readIntMatrix(rows: Int): StdInScanner[Matrix[Int]] =
    readTableWithMap(rows, _.toInt)

  def readBigIntMatrix(rows: Int): StdInScanner[Matrix[BigInt]] =
    readTableWithMap(rows, BigInt(_))
}
