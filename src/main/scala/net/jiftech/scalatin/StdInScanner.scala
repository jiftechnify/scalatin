package net.jiftech.scalatin

/* copy/paste from here... */
import scala.io.StdIn
import scala.reflect.ClassTag

/** Types that can be parsed from string.
  */
trait FromStr[T] {
  def parse(s: String): T
}

object FromStr {
  extension (s: String) {
    def parse[T: FromStr] = summon[FromStr[T]].parse(s)
  }

  given FromStr[String] with {
    def parse(s: String): String = s
  }

  given FromStr[Boolean] with {
    // emulating interpretation of the input used in StdIn.readBoolean
    def parse(s: String): Boolean = s.toLowerCase match {
      case "true" | "t" => true
      case "yes" | "y"  => true
      case _            => false
    }
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

  given [A: FromStr, B: FromStr]: FromStr[(A, B)] with {
    def parse(s: String): (A, B) = {
      val sp = s.split(" ")
      (sp(0).parse, sp(1).parse)
    }
  }

  given [A: FromStr, B: FromStr, C: FromStr]: FromStr[(A, B, C)] with {
    def parse(s: String): (A, B, C) = {
      val sp = s.split(" ")
      (sp(0).parse, sp(1).parse, sp(2).parse)
    }
  }

  given [I[_], A: FromStr](using iterFactory: HasIterFactory[I]): FromStr[I[A]]
    with {
    def parse(s: String): I[A] =
      iterFactory().from(s.split(" ").iterator.map(s => s.parse))
  }

  given [A: ClassTag: FromStr]: FromStr[Array[A]] with {
    def parse(s: String): Array[A] = s.split(" ").map(s => s.parse)
  }

  given [SI[_], A: Ordering: FromStr](using
      iterFactory: HasSortedIterFactory[SI]
  ): FromStr[SI[A]] with {
    def parse(s: String): SI[A] =
      iterFactory().from(s.split(" ").iterator.map(s => s.parse))
  }
}

/** Collection types which have corresponding `IterableFactory`.
  *
  * Call `apply()` method on given value to obtain the `IterableFactory` object.
  */
trait HasIterFactory[I[_]] {
  def apply(): scala.collection.IterableFactory[I]
}

object HasIterFactory {
  given HasIterFactory[Iterator] with {
    def apply() = Iterator
  }
  given HasIterFactory[Vector] with {
    def apply() = Vector
  }

  given HasIterFactory[List] with {
    def apply() = List
  }

  given HasIterFactory[LazyList] with {
    def apply() = LazyList
  }

  given immQueueHasIterFactory: HasIterFactory[scala.collection.immutable.Queue]
    with {
    def apply() = scala.collection.immutable.Queue
  }

  given immSetHasIterFactory: HasIterFactory[Set] with {
    def apply() = Set
  }

  given HasIterFactory[scala.collection.mutable.ArrayBuffer] with {
    def apply() = scala.collection.mutable.ArrayBuffer
  }

  given HasIterFactory[scala.collection.mutable.ListBuffer] with {
    def apply() = scala.collection.mutable.ListBuffer
  }

  given mutQueueHasIterFactory: HasIterFactory[scala.collection.mutable.Queue]
    with {
    def apply() = scala.collection.mutable.Queue
  }

  given HasIterFactory[scala.collection.mutable.ArrayDeque] with {
    def apply() = scala.collection.mutable.ArrayDeque
  }

  given mutSetHasIterFactory: HasIterFactory[scala.collection.mutable.Set]
    with {
    def apply() = scala.collection.mutable.Set
  }
}

/** Sorted collection types which have corresponding `SortedIterableFactory`.
  *
  * Call `apply()` method on given value to obtain the `SortedIterableFactory`
  * object.
  */
trait HasSortedIterFactory[I[_]] {
  def apply(): scala.collection.SortedIterableFactory[I]
}

object HasSortedIterFactory {
  given immSortedSetHasFactory
      : HasSortedIterFactory[scala.collection.immutable.SortedSet] with {
    def apply() = scala.collection.immutable.SortedSet
  }

  given mutSortedSetHasFactory
      : HasSortedIterFactory[scala.collection.mutable.SortedSet] with {
    def apply() = scala.collection.mutable.SortedSet
  }

  given HasSortedIterFactory[scala.collection.mutable.PriorityQueue] with {
    def apply() = scala.collection.mutable.PriorityQueue
  }
}

/** (Unordered) map types which have corresponding `MapFactory`.
  *
  * Call `apply()` method on given value to obtain the `MapFactory` object.
  */
trait HasMapFactory[I[_, _]] {
  def apply(): scala.collection.MapFactory[I]
}

object HasMapFactory {
  given immMapHasFactory: HasMapFactory[Map] with {
    def apply() = Map
  }

  given mutMapHasFactory: HasMapFactory[scala.collection.mutable.Map] with {
    def apply() = scala.collection.mutable.Map
  }
}

/** Sorted map types which have corresponding `SortedMapFactory`.
  *
  * Call `apply()` method on given value to obtain the `SortedMapFactory`
  * object.
  */
trait HasSortedMapFactory[I[_, _]] {
  def apply(): scala.collection.SortedMapFactory[I]
}

object HasSortedMapFactory {
  given immSortedMapHasFactory
      : HasSortedMapFactory[scala.collection.immutable.SortedMap] with {
    def apply() = scala.collection.immutable.SortedMap
  }

  given mutSortedMapHasFactory
      : HasSortedMapFactory[scala.collection.mutable.SortedMap] with {
    def apply() = scala.collection.mutable.SortedMap
  }
}

/** Monadically composable `StdIn` scanner.
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

/** `StdInScanner` building blocks. Compose them using `for`-comprehension to
  * build a scanner for structured text.
  */
object StdInScanner {
  import scala.collection.IterableFactory
  import FromStr.parse

  // read nothing, and return specified "a"
  def of[A](a: A): StdInScanner[A] = StdInScanner(_ => a)

  // read nothing, and return ()
  def unit: StdInScanner[Unit] = StdInScanner.of(())

  // read single line as string
  def readLine: StdInScanner[String] =
    StdInScanner(in => in.readLine.trim)

  // read single line and parse it using FromStr[A].parse()
  def readLineAs[A: FromStr]: StdInScanner[A] =
    StdInScanner(in => in.readLine.trim.parse)

  // read `n` lines as `I[A]`. Each line is parsed using `FromStr[A].parse()`, then collected to collection `I[_]`.
  def readLinesAs[I[_], A: FromStr](n: Int)(using
      iterFactory: HasIterFactory[I]
  ): StdInScanner[I[A]] =
    StdInScanner(in => iterFactory().fill(n)(in.readLine.trim.parse))

  // read `n` lines as `Array[A]`.
  def readLinesAsArray[A: ClassTag: FromStr](n: Int): StdInScanner[Array[A]] =
    StdInScanner(in => Array.fill(n)(in.readLine.trim.parse))

  // read `n` lines as sorted collection (`: SI[A]`). `A` must have ordering (defined by `Ordering[A]`). Each line is parsed using `FromStr[A].parse()`, then collected to collection `SI[_]`.
  def readLinesAsSorted[SI[_], A: Ordering: FromStr](n: Int)(using
      iterFactory: HasSortedIterFactory[SI]
  ): StdInScanner[SI[A]] =
    StdInScanner(in => iterFactory().fill(n)(in.readLine.trim.parse))

  /** read `n` lines as Map (`: M[K, V]`). Each line is parsed using
    * `FromStr[(K, V)].parse()`, then collected to map `M[_, _]`.
    */
  def readLinesAsMap[M[_, _], K: FromStr, V: FromStr](n: Int)(using
      mapFactory: HasMapFactory[M]
  ): StdInScanner[M[K, V]] =
    readLinesAs[Iterator, (K, V)](n).map(kvs => mapFactory().from(kvs))

  // read `n` lines as SortedMap (`: SM[K, V]`). `K` must have ordering (defined by `Ordering[K]`). Each line is parsed using `FromStr[(K, V)].parse()`, then collected to map `SM[_, _]`.
  def readLinesAsSortedMap[SM[_, _], K: Ordering: FromStr, V: FromStr](n: Int)(
      using mapFactory: HasSortedMapFactory[SM]
  ): StdInScanner[SM[K, V]] =
    readLinesAs[Iterator, (K, V)](n).map(kvs => mapFactory().from(kvs))

  // TODO: consider fast Matrix representation?
  type Matrix[A] = Vector[Vector[A]]

  // read `nRows` lines as matrix of type `A`.
  def readMatrixOf[A](nRows: Int)(using
      fs: FromStr[A]
  ): StdInScanner[Matrix[A]] =
    readLinesAs[Vector, Vector[A]](nRows)
}
/* ... to here */

// extension methods of StdInScanner, enables to run scanner on stub input. useful for testing.
object StdInScannerTestingExt {
  import java.io.StringReader
  import java.nio.file.{Files, Paths}
  import scala.util.chaining.scalaUtilChainingOps

  extension [A](sc: StdInScanner[A]) {
    // run entire scanner on the text file
    def runOnFile(filename: String): A = {
      Console.withIn(Paths.get(filename).pipe(Files.newBufferedReader))(
        sc.scan(StdIn)
      )
    }

    // run entire scanner on the String
    def runOnString(str: String): A =
      Console.withIn(new StringReader(str))(sc.scan(StdIn))
  }
}
