package net.jiftech.scalatin

import scala.collection.generic.CanBuild
import scala.io.StdIn
import scala.language.higherKinds

// TODO: write well formed Docs!

case class StdInScanner[A](scan: StdIn.type => A) {
  // transform read result by "f"
  def map[B](f: A => B): StdInScanner[B] =
    StdInScanner(in => f(scan(in)))

  // run this to read A, then obtain new scanner from A using "f", then run the new scanner
  def flatMap[B](f: A => StdInScanner[B]): StdInScanner[B] =
    StdInScanner(in => f(scan(in)).scan(in))

  // run entire scanner and get result
  def run: A = scan(StdIn)
}

object StdInScanner {
  // read nothing, and return specified "a"
  def apply[A](a: A): StdInScanner[A] = StdInScanner(_ => a)
  // read nothing, and return ()
  def unit: StdInScanner[Unit] = StdInScanner(())

  // read a line
  def readLine: StdInScanner[String] = StdInScanner(s => s.readLine().trim)
  // read a line as Int
  def readLnInt: StdInScanner[Int] = StdInScanner(s => s.readInt())
  // read a line as BigInt
  def readLnBigInt: StdInScanner[BigInt] = readLine.map(BigInt(_))

  // read tokens (separated by "sep") from a line, and store them in an array
  def readTokens(sep: String): StdInScanner[Array[String]] =
    readLine.map(_.split(sep).map(_.trim))
  // read tokens (separated by "sep") from a line, and store them in a collection (type "C")
  def readTokensTo[C[_]](sep: String)(implicit cb: CanBuild[String, C[String]]): StdInScanner[C[String]] =
    readTokens(sep).map(_.to[C])


  // read "n" lines, and store them in an array
  def readLines(n: Int): StdInScanner[Array[String]] =
    StdInScanner(s => Array.fill(n)(s.readLine().trim))
  // read "n" lines, and store them in a collection "C"
  def readLinesTo[C[_]](n: Int)(implicit cb: CanBuild[String, C[String]]): StdInScanner[C[String]] =
    readLines(n).map(_.to[C])

  // read "n" ints from lines. and store them in an array
  def readInts(n: Int): StdInScanner[Array[Int]] =
    StdInScanner(s => Array.fill(n)(s.readInt()))
  // read "n" ints from lines. and store them in a collection "C"
  def readIntsTo[C[_]](n: Int)(implicit cb: CanBuild[Int, C[Int]]): StdInScanner[C[Int]] =
    readInts(n).map(_.to[C])

  // read a table of "n" rows
  def readTable(rows: Int, sep: String = " "): StdInScanner[Array[Array[String]]] =
    readLines(rows).map{ lns =>
      lns.map(ln => ln.split(sep).map(tkn => tkn.trim))
    }

  // read a 2D int matrix of "n" rows, stored in an array of arrays
  def readIntMatrix(rows: Int, sep: String = " "): StdInScanner[Array[Array[Int]]] =
    readLines(rows).map{ lns =>
      lns.map(ln => ln.split(sep).map(tkn => tkn.toInt))
    }

  // read a 2D int matrix of "n" rows, stored in a collection of collections ("C[C[Int]")
  def readIntMatrixTo[C[_]](rows: Int, sep: String = " ")(implicit cb1: CanBuild[Int, C[Int]] ,cb2: CanBuild[C[Int], C[C[Int]]]): StdInScanner[C[C[Int]]] =
    readIntMatrix(rows, sep).map { mat =>
      mat.map(row => row.to[C]).to[C]
    }
}
