// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package adpro

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail: Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f: (A, =>B) => B): B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


  // Exercise 2

  def toList: List[A] = foldLeft(List.empty: List[A])((cur, acc) => acc :+ cur );

  // Exercise 3

  def take (n: Int): Stream[A] = this match {
    case Cons(h, t) => if(n == 0) Empty else cons(h(), t().take(n - 1));
    case _ => Empty;
  }


  def drop (n: Int): Stream[A] = this match {
    case Cons(h, t) => if(n > 0) t().drop(n - 1) else this;
    case _ => Empty;
  }


  // Exercise 4

  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if(p(h())) cons(h(), t().takeWhile(p)) else Empty;
    case _ => Empty;
  }


  //Exercise 5
  // naturals.forAll(_ >= 0) - Every element in naturals are larger than 0, therefore it will never exit
  def forAll (p: A => Boolean): Boolean = !this.exists(x => !p(x))


  //Exercise 6
  // no infinity support
  def takeWhile2 (p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((cur, acc) => if (p(cur)) cons(cur, acc) else Empty)

  //Exercise 7

  def headOption2: Option[A] = foldRight(None: Option[A])((cur, _) => Some(cur))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises

  def map[B] (f: A => B): Stream[B] = foldRight(Empty: Stream[B])((curt, acc) => cons(f(curt), acc))

  // scala> naturals.map (_*2).drop (30).take (50).toList
  // val res0: List[Int] = List(62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144, 146, 148, 150, 152, 154, 156, 158, 160)


  def filter (p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((curt, acc) => if (p(curt)) cons(curt, acc) else acc)

  // scala> naturals.drop (42).filter (_%2 ==0).take (30).toList
  // val res1: List[Int] = List(44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102)


  // we are not really sure why we need the syntax 'B >: A' - can you explain?  
  def append[B >: A] (that: => Stream[B]): Stream[B] = foldRight(that)((cur, acc) => cons(cur, acc))

  // Gives a stack overflow error when run with the case from the exercises -> we are not sure why
  def flatMap[B >: A] (f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((cur, acc) => acc.append(() => f(cur)))

  // note: Ctrl-h: 'kuken' -> 'cur'
  // note: Ctrl-h: 'curt' -> 'kuken'

  //Exercise 09
  //Put your answer here:
  /*
    For a List, the implementation would cause every element of the List to be evaulauted by the call to 'filter'. But since
    streams are lazy, we only evaluate elements until we find an 'x' where p(x) is true, instead of computing p(x) for all elements
    (as we would do with the List implementation).

  */

  //Exercise 10
  //Put your implementatino here:
  def fifibb: Stream[Int] = 



  // Exercise 13

  def map_ = ???
  def take_ = ???
  def takeWhile_ = ???
  def zipWith_ = ???

}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = cons(n, from(n+1));

  def to (n: Int): Stream[Int] = cons(n, if(n == 0) Empty else to(n-1));

  val naturals: Stream[Int] = from(1);

  //Exercise 11

  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] = ???

  // Exercise 12

  def fib2  = ???
  def from2 = ???

}
