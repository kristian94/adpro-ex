// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// Work on this file by following the associated exercise sheet
// (available in PDF in the same directory).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain'.
// To load the file int the REPL use the 'console' command.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests (for the solved exercises), 
// after you are done with each exercise (if you do them in order).  
// Compile and test frequently. Best continously.

package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 1 requires no programming
  // answer: 3

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => throw new IllegalArgumentException("Empty list supplied")
    case Cons (x,xs) => xs
  }

  // Exercise 3

  
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  @annotation.tailrec
  def drop[A] (l: List[A], n: Int) : List[A] =  {
    if(n == 0) l else drop(tail(l), n - 1)
  }

  // Exercise 4

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case _ => if(f(List.head(l))) dropWhile(tail(l), f) else l
  }

  // Exercise 5

  // def init[A] (l: List[A]): List[A] = ???
  def init[A] (l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Empty list supplied")
    case Cons(x, Nil) => Nil
    case _ => {
        def go(m: List[A]): List[A] = m match {
          case Nil => Nil
          case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
          case Cons(x, y) => Cons(x, go(y))
        }
        Cons(List.head(l), go(tail(l)))
      }
  }

  // Exercise 6

  def length[A] (as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)
  }

  // def length[A] (as: List[A]): Int = ???

  // Exercise 7



  // foldLeft (List (1,2,3,4),0) (_ + _) computes (((0 + 1) + 2) + 3) + 4 while
  @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match {
    case Cons(x, Nil) => f(z, x)
    case Nil => z
    case Cons(x, y) => foldLeft(y, f(z, x))(f)
  }

  // Exercise 8

  def product (as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def length1[A] (as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)
  }

  // Exercise 9

  def reverse[A] (as: List[A]): List[A] = foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))

  // Exercise 10

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = foldLeft(reverse(as), z)((x, y) => f(y, x))

  // Exercise 11

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B = 
    foldRight[A, B => B](as, b => b)((a: A, acc: B => B) => b => acc(f(b, a)))(z)

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  def concat[A] (as: List[List[A]]): List[A] = foldRight(as, Nil: List[A])(append)

  // Exercise 13

  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case _ => if(f(List.head(as))) Cons(List.head(as), filter(tail(as))(f)) else filter(tail(as))(f)
  }

  // Exercise 14
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  // Exercise 15

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = flatMap(l)(x => if(p(x)) Cons(x, Nil) else Nil)

  // Exercise 16

  def add (l: List[Int]) (r: List[Int]): List[Int] = 
    if(l != Nil && r != Nil) Cons(List.head(l) + List.head(r), add(tail(l))(tail(r))) else Nil

  // Exercise 17

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] =
    if(l != Nil && r != Nil) Cons(f(List.head(l), List.head(r)), zipWith(f)(tail(l), tail(r))) else Nil

  // Exercise 18

  def every[A] (as: List[A]) (p: A => Boolean) = foldLeft(as, true)((a, b) => a == true && p(b));

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = 
    if (sub == Nil) true
    else if (sup == Nil) false
    else if (every(zipWith((a: A, b: A) => a == b)(sup, sub))(_ == true)) true
    else hasSubsequence(tail(sup), sub);

  // Exercise 19

  // y: row#, from top to bottom
  // x: col#, from left to right

  def pascal (n: Int): List[Int] = {
    def xth(x: Int, y: Int): Int = {
      if(x == y) 1
      else if(x == 0) 1
      else xth(x-1, y-1) + xth(x, y-1)
    }

    def go(curX: Int): List[Int] = {
      if(curX == n) Nil else Cons(xth(curX, n - 1), go(curX + 1))
    }
    if(n == 0) Nil else Cons(1, go(1));
  }

}
