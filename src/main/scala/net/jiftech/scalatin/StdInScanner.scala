package net.jiftech.scalatin

/* copy/paste from here... */
import scala.io.StdIn

/**
 * Type that can be parsed from string.
 */
trait FromStr[T] {
  def parse(s: String): T
}

object FromStr {
  given FromStr[String] with {
    def parse(s: String): String = s
  }

  given FromStr[Int] with {
    def parse(s: String): Int = s.toInt
  }

  given FromStr[Long] with {
    def parse(s: String): Long = s.toLong
  }

  given FromStr[BigInt] with {
    def parse(s: String): BigInt = BigInt(s)
  }

  given FromStr[Double] with {
    def parse(s: String): Double = s.toDouble
  }

  given FromStr[BigDecimal] with {
    def parse(s: String): BigDecimal = BigDecimal(s)
  }
}

/**
 * Collection type constructor which has corresponding `IterableFactory`.
 * 
 * Call `apply()` method on given value to obtain the `IterableFactory` object.
 */
trait IterFactory[CC[_]] {
  def apply(): scala.collection.IterableFactory[CC]
}

object IterFactory {
  given IterFactory[Iterator] with {
    def apply() = Iterator
  }

  given IterFactory[Vector] with {
    def apply() = Vector
  }

  given IterFactory[List] with {
    def apply() = List
  }

  given immutableSetIterFactory: IterFactory[Set] with {
    def apply() = Set
  }

  given IterFactory[scala.collection.mutable.ArrayBuffer] with {
    def apply() = scala.collection.mutable.ArrayBuffer
  }

  given IterFactory[scala.collection.mutable.Stack] with {
    def apply() = scala.collection.mutable.Stack
  }

  given IterFactory[scala.collection.mutable.Queue] with {
    def apply() = scala.collection.mutable.Queue
  }

  given IterFactory[scala.collection.mutable.ArrayDeque] with {
    def apply() = scala.collection.mutable.ArrayDeque
  }

  given mutableSetIterFactory: IterFactory[scala.collection.mutable.Set] with {
    def apply() = scala.collection.mutable.Set
  }
}

/**
 * Monadically composable text scanner read from `StdIn`.
 */
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

/**
 * `StdInScanner` building blocks. Compose them using `for`-comprehension to build a scanner for structured text.
 */
object StdInScanner {
  import scala.collection.IterableFactory
  import FromStr.given
  import IterFactory.given

  /* pure(read nothing) scanners */
  // read nothing, and return specified "a"
  def of[A](a: A): StdInScanner[A] = StdInScanner(_ => a)
  // read nothing, and return ()
  def unit: StdInScanner[Unit] = StdInScanner.of(())

  /* read single line -> produce single value */
  // read single line
  def readLine: StdInScanner[String] = StdInScanner(s => s.readLine().trim)

  // read single line as T: FromStr
  def read[A](using fs: FromStr[A]): StdInScanner[A] = StdInScanner(s => fs.parse(s.readLine)) 
  
  // read single line as a pair of values
  def readPairWithMap[A, B](fa: String => A, fb: String => B): StdInScanner[(A, B)] =
    readTokens[Vector].map(v => (fa(v(0)), fb(v(1))))

  //
  def readPairOf[A, B](using fsA: FromStr[A], fsB: FromStr[B]): StdInScanner[(A, B)] =
    readPairWithMap(fsA.parse, fsB.parse)

  //
  def readPairOfSameType[A](using fs: FromStr[A]): StdInScanner[(A, A)] =
    readPairWithMap(fs.parse, fs.parse)

  // read single line as a triple of values
  def readTripleWithMap[A, B, C](fa: String => A, fb: String => B, fc: String => C): StdInScanner[(A, B, C)] =
    readTokens[Vector].map(v => (fa(v(0)), fb(v(1)), fc(v(2))))

  //
  def readTripleOf[A, B, C](using fsA: FromStr[A], fsB: FromStr[B], fsC: FromStr[C]): StdInScanner[(A, B, C)] =
    readTripleWithMap(fsA.parse, fsB.parse, fsC.parse)

  def readTripleOfSameType[A](using fs: FromStr[A]): StdInScanner[(A, A, A)] =
    readTripleWithMap(fs.parse, fs.parse, fs.parse)

  /* read single line -> produce multiple values */
  // read tokens from a line, then map them with "f"
  def readTokensWithMap[CC[_], A](f: String => A)(using iterFactory: IterFactory[CC]): StdInScanner[CC[A]] =
    readLine.map(s => iterFactory().from(s.split(" ").iterator.map(f)))

  //
  def readTokens[CC[_]: IterFactory]: StdInScanner[CC[String]] =
    readTokensWithMap(identity)

  //
  def readTokensOf[CC[_]: IterFactory, A](using fs: FromStr[A]): StdInScanner[CC[A]] =
    readTokensWithMap(fs.parse)

  
  /* read multiple lines -> produce one value per line */
  def readLinesWithMap[CC[_], A](n: Int, f: String => A)(using iterFactory: IterFactory[CC]): StdInScanner[CC[A]] =
    StdInScanner(s => iterFactory().fill(n)(f(s.readLine())))

  //
  def readLines[CC[_]: IterFactory](n: Int): StdInScanner[CC[String]] =
    readLinesWithMap(n, identity)

  //
  def readLinesOf[CC[_]: IterFactory, A](n: Int)(using fs: FromStr[A]): StdInScanner[CC[A]] =
    readLinesWithMap(n, fs.parse)
  
  //
  def readPairsWithMap[CC[_], A, B](n: Int)(fa: String => A, fb: String => B)(using iterFactory: IterFactory[CC]): StdInScanner[CC[(A, B)]] =
    readLines[Iterator](n).map{ strIter => 
      iterFactory().from(strIter.map{ line =>
        val t1 :: t2 :: _ = line.split(" ").take(2).toList
        (fa(t1), fb(t2))
      }
    )}
  //
  def readPairsOf[CC[_]: IterFactory, A, B](n: Int)(using fsA: FromStr[A], fsB: FromStr[B]): StdInScanner[CC[(A, B)]] =
    readPairsWithMap(n)(fsA.parse, fsB.parse)

  //
  def readPairsOfSameType[CC[_]: IterFactory, A](n: Int)(using fs: FromStr[A]): StdInScanner[CC[(A, A)]] =
    readPairsWithMap(n)(fs.parse, fs.parse)

  //
  def readMapOf[K, V](n: Int)(using fsK: FromStr[K], fsV: FromStr[V]): StdInScanner[Map[K, V]] =
    readPairsOf[Iterator, K, V](n).map(pairIter => Map.from(pairIter))

  //
  def readMutMapOf[K, V](n: Int)(using fsK: FromStr[K], fsV: FromStr[V]): StdInScanner[scala.collection.mutable.Map[K, V]] =
    readPairsOf[Iterator, K, V](n).map(pairIter => scala.collection.mutable.Map.from(pairIter))

  //
  def readSortedMapOf[K: Ordering, V](n: Int)(using fsK: FromStr[K], fsV: FromStr[V]): StdInScanner[scala.collection.immutable.SortedMap[K, V]] =
    readPairsOf[Iterator, K, V](n).map(pairIter => scala.collection.immutable.SortedMap.from(pairIter))

  //
  def readMutSortedMapOf[K: Ordering, V](n: Int)(using fsK: FromStr[K], fsV: FromStr[V]): StdInScanner[scala.collection.mutable.SortedMap[K, V]] =
    readPairsOf[Iterator, K, V](n).map(pairIter => scala.collection.mutable.SortedMap.from(pairIter))

  //
  def readTriplesWithMap[CC[_], A, B, C](n: Int)(fa: String => A, fb: String => B, fc: String => C)(using iterFactory: IterFactory[CC]): StdInScanner[CC[(A, B, C)]] =
    readLines[Iterator](n).map{ strIter => 
      iterFactory().from(strIter.map{ line =>
        val t1 :: t2 :: t3 :: _ = line.split(" ").take(3).toList
        (fa(t1), fb(t2), fc(t3))
      }
    )}

  //
  def readTriplesOf[CC[_]: IterFactory, A, B, C](n: Int)(using fsA: FromStr[A], fsB: FromStr[B], fsC: FromStr[C]): StdInScanner[CC[(A, B, C)]] =
    readTriplesWithMap(n)(fsA.parse, fsB.parse, fsC.parse)

  //
  def readTriplesOfSameType[CC[_]: IterFactory, A](n: Int)(using fs: FromStr[A]): StdInScanner[CC[(A, A, A)]] =
    readTriplesWithMap(n)(fs.parse, fs.parse, fs.parse)


  // /* read multiple lines -> produce multiple value per line */
  type Matrix[A] = Vector[Vector[A]]

  def readMatrixWithMap[A](nRows: Int, f: String => A): StdInScanner[Matrix[A]] =
    readLines[Vector](nRows).map{ lns =>
      lns.map(ln => ln.split(" ").toVector.map(tk => f(tk)))
    }

  def readMatrix(nRows: Int): StdInScanner[Matrix[String]] =
    readMatrixWithMap(nRows, identity)

  def readMatrixOf[A](nRows: Int)(using fs: FromStr[A]): StdInScanner[Matrix[A]] =
    readMatrixWithMap(nRows, fs.parse)
}
/* ... to here */

// extension methods of StdInScanner, enables to run scanner on stub input. useful for testing.
object StdInScannerSyntaxForTesting {
  import java.io.StringReader
  import java.nio.file.{Files, Paths}
  
  extension [A](sc: StdInScanner[A]) {
    // run entire scanner on the text file
    def runOnFile(filename: String): A =
    Console.withIn(Files.newBufferedReader(Paths.get(filename)))(sc.scan(StdIn))

    // run entire scanner on the String
    def runOnString(str: String): A =
    Console.withIn(new StringReader(str))(sc.scan(StdIn))
  }
}
