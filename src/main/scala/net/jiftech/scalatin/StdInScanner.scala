package net.jiftech.scalatin

/* copy/paste from here... */
import scala.io.StdIn
import scala.reflect.ClassTag

/** Type class of generic collection types which have corresponding `IterableFactory`.
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

  given immQueueHasIterFactory: HasIterFactory[scala.collection.immutable.Queue] with {
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

  given mutQueueHasIterFactory: HasIterFactory[scala.collection.mutable.Queue] with {
    def apply() = scala.collection.mutable.Queue
  }

  given HasIterFactory[scala.collection.mutable.ArrayDeque] with {
    def apply() = scala.collection.mutable.ArrayDeque
  }

  given mutSetHasIterFactory: HasIterFactory[scala.collection.mutable.Set] with {
    def apply() = scala.collection.mutable.Set
  }
}

/** Type class of generic sorted collection types which have corresponding `SortedIterableFactory`.
  */
trait HasSortedIterFactory[I[_]] {
  def apply(): scala.collection.SortedIterableFactory[I]
}

object HasSortedIterFactory {
  given immSortedSetHasFactory: HasSortedIterFactory[scala.collection.immutable.SortedSet] with {
    def apply() = scala.collection.immutable.SortedSet
  }

  given mutSortedSetHasFactory: HasSortedIterFactory[scala.collection.mutable.SortedSet] with {
    def apply() = scala.collection.mutable.SortedSet
  }

  given HasSortedIterFactory[scala.collection.mutable.PriorityQueue] with {
    def apply() = scala.collection.mutable.PriorityQueue
  }
}

/** Type class of collection types which can only hold value of type `A` and have corresponding
  * `SpecificIterableFactory`.
  *
  * Quintessential instance of this type class is `BitSet` (`BitSet: HasSpecificIterFactory[BitSet, Int]`).
  */
trait HasSpecificIterFactory[I] {
  type Elem
  def apply(): scala.collection.SpecificIterableFactory[Elem, I]
}

object HasSpecificIterFactory {
  given immBitSetHasFactory: HasSpecificIterFactory[scala.collection.immutable.BitSet] with {
    type Elem = Int
    def apply() = scala.collection.immutable.BitSet
  }

  given mutBitSetHasFactory: HasSpecificIterFactory[scala.collection.mutable.BitSet] with {
    type Elem = Int
    def apply() = scala.collection.mutable.BitSet
  }
}

/** Generic map types which have corresponding `MapFactory`.
  */
trait HasMapFactory[I[_, _]] {
  def apply(): scala.collection.MapFactory[I]
}

object HasMapFactory {
  given immMapHasFactory: HasMapFactory[Map] with {
    def apply() = scala.collection.immutable.Map
  }

  given mutMapHasFactory: HasMapFactory[scala.collection.mutable.Map] with {
    def apply() = scala.collection.mutable.Map
  }
}

/** Geneic sorted map types which have corresponding `SortedMapFactory`.
  */
trait HasSortedMapFactory[I[_, _]] {
  def apply(): scala.collection.SortedMapFactory[I]
}

object HasSortedMapFactory {
  given immSortedMapHasFactory: HasSortedMapFactory[scala.collection.immutable.SortedMap] with {
    def apply() = scala.collection.immutable.SortedMap
  }

  given mutSortedMapHasFactory: HasSortedMapFactory[scala.collection.mutable.SortedMap] with {
    def apply() = scala.collection.mutable.SortedMap
  }
}

/** `HasFactory` is a type class of concrete collection types (e.g. `List[String]`, `Map[String, Int]`, `BitSet`) that
  * has corresponding `scala.collection.Factory`.
  *
  * Abstract type `Elem` denotes the "element" type of the given collection type `C`.
  *
  * Value of type `C` can be constructed from a sequence of value of type `Elem` (i.e. `IterableOnce[Elem]`) using
  * `scala.collection.Factory[Elem, C]` which can be obtained by `apply()` method.
  */
trait HasFactory[C] {
  type Elem
  def apply(): scala.collection.Factory[Elem, C]
}

object HasFactory {
  given hasIterFactory[I[_], A](using
      iterFactory: HasIterFactory[I]
  ): HasFactory[I[A]] with {
    type Elem = A
    def apply() = iterFactory()
  }

  given hasSortedIterFactory[I[_], A: Ordering](using
      iterFactory: HasSortedIterFactory[I]
  ): HasFactory[I[A]] with {
    type Elem = A
    def apply() = iterFactory()
  }

  given hasSpecificIterFactory[I](using
      iterFactory: HasSpecificIterFactory[I]
  ): HasFactory[I] with {
    type Elem = iterFactory.Elem
    def apply() = iterFactory()
  }

  given hasMapFactory[M[_, _], K, V](using
      mapFactory: HasMapFactory[M]
  ): HasFactory[M[K, V]] with {
    type Elem = (K, V)
    def apply() = mapFactory()
  }

  given hasSortedMapFactory[M[_, _], K: Ordering, V](using
      mapFactory: HasSortedMapFactory[M]
  ): HasFactory[M[K, V]] with {
    type Elem = (K, V)
    def apply() = mapFactory()
  }

  given [A: ClassTag]: HasFactory[Array[A]] with {
    type Elem = A
    def apply() = Array
  }
}

/** Types that can be parsed from string.
  */
trait FromStr[T] {
  def parse(s: String): T
}

object FromStr {
  extension (s: String) {

    /** Enable `val a: A = "str".parse` syntax when `A: FromStr`.
      */
    def parse[T](using fs: FromStr[T]) = fs.parse(s)
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

  given iter[I[_], A: FromStr](using
      iterFactory: HasIterFactory[I]
  ): FromStr[I[A]] with {
    def parse(s: String): I[A] =
      iterFactory().from(s.split(" ").iterator.map(s => s.parse))
  }

  given sortedIter[I[_], A: Ordering: FromStr](using
      iterFactory: HasSortedIterFactory[I]
  ): FromStr[I[A]] with {
    def parse(s: String): I[A] =
      iterFactory().from(s.split(" ").iterator.map(s => s.parse))
  }

  given specificIter[I](using
      iterFactory: HasSpecificIterFactory[I],
      fs: FromStr[iterFactory.Elem]
  ): FromStr[I] with {
    def parse(s: String): I =
      iterFactory().fromSpecific(s.split(" ").iterator.map(s => s.parse))
  }

  given [A: ClassTag: FromStr]: FromStr[Array[A]] with {
    def parse(s: String): Array[A] = s.split(" ").map(s => s.parse)
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

/** `StdInScanner` building blocks. Compose them using `for`-comprehension to build a scanner for structured text.
  */
object StdInScanner {
  import FromStr.parse

  // read nothing, return given value
  def pure[A](a: A): StdInScanner[A] = StdInScanner(_ => a)

  // read single line as string
  def readLine: StdInScanner[String] =
    StdInScanner(in => in.readLine.trim)

  // read single line and parse it as type `A`
  def readLineAs[A: FromStr]: StdInScanner[A] =
    StdInScanner(in => in.readLine.trim.parse)

  // read `n` lines as collection type `C`.  parse each line as element type of `C`, then construct collection `C` from sequence of parsed values
  def readLinesAs[C](n: Int)(using
      factory: HasFactory[C],
      fs: FromStr[factory.Elem]
  ): StdInScanner[C] =
    StdInScanner(in => Iterator.fill(n)(in.readLine.trim.parse).to(factory()))

  // TODO: consider fast Matrix representation?
  type Matrix[A] = Vector[Vector[A]]

  // read `nRows` lines as matrix of type `A`.
  def readMatrixOf[A](nRows: Int)(using
      fs: FromStr[A]
  ): StdInScanner[Matrix[A]] =
    readLinesAs[Vector[Vector[A]]](nRows)
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
