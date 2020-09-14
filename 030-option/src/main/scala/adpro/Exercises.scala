// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: k_____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.  The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {
  
  this: java.awt.Point =>

/*
  this > that  -> 1
  this < that  -> -1
  this = that  -> 0

  (this.x, this.y) < (that.x, that.y) iff this.x < that.x or (this.x = that.x and this.y < that.y)
*/
  override def compare (that: java.awt.Point): Int = {
    if      (this.x <= that.x && this.y < that.y)   -1
    else if (this.x == that.x && this.y == that.y)  0
    else                                            1
  }
}



// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)
// donnerz

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2

  def size[A] (t :Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }
    
  
  // Exercise 3

  def maximum (t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  // Exercise 4

  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 5

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = t match {
    case Leaf(x) => g(x)
    case Branch(left, right) => f(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size1[A] (t: Tree[A]): Int = fold(t)((a: Int, b: Int) => a + b + 1)(_ => 1)

  def maximum1 (t: Tree[Int]): Int = fold(t)((x: Int, y: Int) => x.max(y))(x => x)

  def map1[A,B] (t: Tree[A]) (f: A => B): Tree[B] = fold(t)((x: Tree[B], y: Tree[B]) => Branch(x, y): Tree[B])(x => Leaf(f(x)))
}

sealed trait Option[+A] {

  // Exercise 6

  def map[B] (f: A=>B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  /**
   * Ignore the arrow (=>) in default's type below for now.
   * It prevents the argument "default" from being evaluated until it is needed.
   * So it is not evaluated if we process a Some object (this is 'call-by-name' 
   * and we should talk about this soon). 
   */

  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B] (f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def filter (p: A => Boolean): Option[A] = this match {
    case Some(x) if p(x) => Some(x)
    case _ => None
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7

  // def variance (xs: Seq[Double]): Option[Double] = {
  //   val m = mean(xs)
  //   val variances = xs.flatMap(x => math.pow(x - m, 2))
  //   val sum = reduce()
  //   return sum / xs.length
  // }
  
  /*
    m = mean(sequence)
    for each x in sequence
      math.pow(x - m, 2)
  */

  def variance (xs: Seq[Double]): Option[Double] = {
    for {
      m <- mean(xs)
      variances = xs.map(x => math.pow(x - m, 2))
    } yield ( mean(variances).getOrElse(0) )
  } // We figured it out, dont say it!!

  // Exercise 8

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = {
    for {
      a <- ao
      b <- bo
    } yield (f(a, b))
  }


  // Exercise 9

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] = aos.foldRight(Some(List.empty): Option[List[A]])((cur: Option[A], acc: Option[List[A]]) => acc.flatMap(x => cur.map(c => x.prepended(c))))
  
  // flipped version:
  // def sequence[A] (aos: List[Option[A]]): Option[List[A]] = aos.foldLeft(Some(List.empty): Option[List[A]])((acc: Option[List[A]], cur: Option[A]) => acc.flatMap(x => cur.map(c => x.appended(c))))


  // Exercise 10

  def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] = {
    as.foldRight(Some(List.empty): Option[List[B]])( (cur: A, acc: Option[List[B]]) => acc.flatMap(x => f(cur).map(c => x.prepended(c))))
  }
}
