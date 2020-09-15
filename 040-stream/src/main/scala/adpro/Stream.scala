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

  def map[B] (f: A => B): Stream[B] = foldRight(Empty: Stream[B])((cur, acc) => cons(f(cur), acc))

  // scala> naturals.map(_*2).drop (30).take (50).toList
  // val res0: List[Int] = List(62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144, 146, 148, 150, 152, 154, 156, 158, 160)


  def filter (p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((cur, acc) => if (p(cur)) cons(cur, acc) else acc)

  // scala> naturals.drop (42).filter (_%2 ==0).take (30).toList
  // val res1: List[Int] = List(44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102)


  // we are not really sure why we need the syntax 'B >: A' - can you explain?  
  def append[B >: A] (that: => Stream[B]): Stream[B] = foldRight(that)((cur, acc) => cons(cur, acc))

  // Gives a stack overflow error when run with the case from the exercises -> we are not sure why
  def flatMap[B >: A] (f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((cur, acc) => acc.append(f(cur)))

  //Exercise 09
  //Put your answer here:
  /*
    For a List, the implementation would cause every element of the List to be evaulauted by the call to 'filter'. But since
    streams are lazy, we only evaluate elements until we find an 'x' where p(x) is true, instead of computing p(x) for all elements
    (as we would do with the List implementation).

  */

  // //Exercise 10
  // //Put your solution here:
  // def fib: Stream[Int] = from(0)
// 
  // def fifibb: Stream[Int] = this match {
  //   case Empty => cons(0, cons(1, fifibb))
  //   case Cons(x: () => Int, Cons(y: () => Int, _)) => cons(x(), cons(y(), cons(x() + y(), fifibb)))
  // }
// fifibb().take(5) = Stream{0,1,1,2,3}



  // Exercise 13


  def map_[B] (f: A => B): Stream[B] = unfold(this)(s => s.headOption.map(h => (f(h), s.tail)))
  // scala> naturals.map_(_*2).drop (15).take (10).toList
  // val res2: List[Int] = List(32, 34, 36, 38, 40, 42, 44, 46, 48, 50)


  def take_ (n: Int): Stream[A] = unfold((this, 0))(s => {
    val (stream, index) = s
    if(index == n) None
    else stream.headOption.map(h => (h, (stream.tail, index + 1)))  
  })
  // scala> naturals.map_(_*2).drop(15).take_(5).toList
  // val res1: List[Int] = List(32, 34, 36, 38, 40)


  def takeWhile_(p: A => Boolean): Stream[A] = unfold((this, 0))(s => {
    val (stream, index) = s
    stream.headOption.flatMap(h => if(p(h)) Some(h, (stream.tail, index + 1)) else None)  
  })
  // scala> naturals.map_(_*2).takeWhile_(x => x < 30).toList
  // val res0: List[Int] = List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28)

  
  def zipWith_[B](that: Stream[B]): Stream[(A, B)] = unfold((this, that))(s => {
    val (curA, curB) = s
    for {
      ha <- curA.headOption
      hb <- curB.headOption
    } yield ( ( (ha, hb), (curA.tail, curB.tail) ) )
  })
  // scala> naturals.zipWith_(naturals).take(5).toList
  // val res1: List[(Int, Int)] = List((1,1), (2,2), (3,3), (4,4), (5,5))

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

  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] = {
    val option = f(z)
    if(option != None) cons(option.get._1, unfold(option.get._2)(f)) else Empty
  }

  val naturals2 = unfold(0)(s => Some((s, s+1)));

  // Exercise 12

  def fib2 = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def from2 (n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))
  
}
