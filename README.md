# Scalatin
Simple monadic input scanner library for Scala. 

Scalatin is meant to be used in competitive programming i.e. [AtCoder](https://atcoder.jp/). 

## Example

```scala
import StdInScanner._

// input:
// 1
// 2 3
// test

val scanner = for {
  a      <- readInt
  (b, c) <- readIntPair
  s      <- readLine
} yield (a+b+c, s)

val (sum, s) = scanner.run
// sum = 6
// s = "test"
```

## Usage
- In code submission, copy/paste library code block indicated by comment.
- For testing, `StdInScannerSyntaxForTesting` might be useful. It adds some extension methods to `StdInScanner` that enable to run scanner on stub input(file or string litral).
  + `#runOnFile(filename)`: run scanner on the specified file.
  + `#runOnString(str)`: run scanner on the supplied string. 

## List of All Scanners
### Pure scanners(read nothing)
- `StdinScanner.of[A](a: A)`: Always emit constant `a`. Read nothing from input.

```scala
StdInScanner.of(1).run
// 1
```

```scala
// input:
// hoge

val sc = for {
  c <- StdInScanner.of(42)
  s <- readLine
} yield (c, s)

val (c, s) = sc.run
// c = 42
// s = "hoge"
```

- `unit`: always emit `()`(value of type `Unit`). Read nothing from input.

### Read single line, produce single value
- `readLine`: Read a line as `String`. 

- `read{Int/Long/BigInt}`: Read a line as `Int/Long/BigInt`. 

```scala
// input:
// 1234
// hoge

val sc = for {
  i <- readInt
  s <- readLine
} yield (i, s)

val (i, s) = sc.run
// i = 1234
// s = "hoge"
```

- `readPair[A, B](fa, fb)`: Read a line as a pair of strings(separated by a whitespace) and convert each element by the supplied functions `fa`, `fb`.

- `read{String/Int/Long}Pair`: Read a line as a pair of `String/Int/Long` s.


```scala
// input:
// hoge true

val (a, b) = readPair(_.toUpperCase, _.toBoolean).run
// a = "HOGE"
// b = true: Boolean
```

```scala
// input:
// 100 200

val (m, n) = readIntPair.run
// m = 100
// n = 200
```

- `readTriple[A, B, C](fa, fb, fc)`: Read a line as a triple of strings(sep. by a whitespace) and convert each element by supplied functions.

- `read{String/Int/Long}Triple`: Read a line as a triple of `String/Int/Long` s.

### Read single line, produce list of values
> note: Container type of produced values is `Vector` by default.

- `readTokensWithMap[A](f)`: Read a line as a list of tokens(strings sep. by whitespace) and map over the list by the supplied function.

- `readTokens` / `read{Int/Long/BigInt}Tokens`: Read a line as a list of `String/Int/Long/BigInt`.

```scala
// input:
// 1 1 2 3 5 8

val v = readIntTokens.run
// v =  Vector(1, 1, 2, 3, 5, 8)
```

### Read multiple lines, produce list of values
> note: Container type of produced values is `Vector` by default.

- `readLinesWithMap[A](n: Int, f)`: Read `n` lines as a list of strings and map over the list by the supplied function.

- `readLines(n: Int)` / `read{Ints/Longs/BigInts}(n: Int)`: Read `n` lines as a list of `String/Int/Long/BigInt` s.

```scala
// input:
// 3 2
// 123
// 456
// 789
// hoge
// fuga

val sc = for {
  (m, n) <- readIntPair
  a <- readInts(m)
  b <- readLines(n)
} yield (a, b)

val (a, b) = sc.run
// a = Vector(123, 456, 789)
// b = Vector("hoge", "fuga")
```

- `readPairs[A, B](n: Int)(fa, fb)`: Read `n` lines as a list of pairs(sep. by whitespace) and convert each element of pairs in the list by the supplied functions.

- `readIntPairs(n: Int)`: Read `n` lines as a list of `Int`'s pairs.

```scala
// input:
// a 1
// b 2
// c 3

val ps = readPairs(3)(_.toUpperCase, _.toInt).run
// ps = Vector(("A", 1), ("B", 2), ("C", 3))
```

- `readTriples[A, B, C](n: Int)(fa, fb, fc)` / `readIntTriples(n: Int)`: analogous to `readPairs` / `readIntPairs`, produce triples instead of pairs.

### Read multiple lines, produce matrix of values
> note: Container type of produced values is `type Matrix[A] = Vector[Vector[A]]` by default.

- `readMatrixWithMap[A](nRows: Int, f)`: Read `n` lines as matrix(each line corresponds a row, each token corresponds a column of the row), and map over the matrix by the supplied function.

- `readMatrix(nRows: Int)` / `read{Int/Long/BigInt}Matrix(nRows: Int)`: Read `n` lines as matrix of `String/Int/Long/BigInt` s.

```scala
// input:
// 3
// 1 2 3
// 4 5 6
// 7 8 9

val sc = for {
  nRows <- readInt
  mat <- readIntMatrix(nRows)
} yield mat

val mat = sc.run
// mat = Vector(
//  Vector(1, 2, 3),
//  Vector(4, 5, 6),
//  Vector(7, 8, 9))
```

## Notice
- If the input can't be parsed as the type specified by the scanner(i.e. `Int` in `readInt/readInts` etc.), it just throws exception. Since Scalatin is meant to be used only to read competitive programming's inputs, sophisticated error handling system is not provided. 

## License
Licensed under [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0).