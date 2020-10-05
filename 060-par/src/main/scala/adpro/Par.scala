// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: Team (k|ch)rist(ian|(o(ph|ff)er))
//
// AUTHOR1: Christopher Borum
// TIME1: ca 4-5 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Kristoffer Noga
// TIME2: ca 4-5 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR3: Kristian Nielsen
// TIME3: ca 4-5 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)

package adpro

import java.util.concurrent.{Executors,ExecutorService,CountDownLatch,TimeUnit,Callable}
import scala.language.implicitConversions
import scala.io.Source

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

trait Future[+A] {
  private[adpro] def apply(k: A => Unit): Unit
}



object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new java.util.concurrent.atomic.AtomicReference[A] 
    val latch = new CountDownLatch(1) 
    p(es) { a => ref.set(a); latch.countDown } 
    latch.await 
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    def apply(k: A => Unit) = f(k)
  }

  def eval (es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  def map2[A,B,C] (p: Par[A], p2: Par[B]) (f: (A,B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a,br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }
  
  // map is shown in the blocking part of the book (should still work but
  // probably uses one thread more than the version  below

  // def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
  //   map2 (pa,unit (())) ((a,_) => f(a))
  
  // This is the version of map2 specialized for nonblocking (Section 7.4.4
  // version)

  def map[A,B] (p: Par[A]) (f: A => B): Par[B] =
    es => new Future[B] {
      def apply (cb: B => Unit): Unit =
        p (es) ( a => eval (es) { cb (f (a)) } )
    }

  // Exercise 1
  //
  // Write the answer here in a comment.
  //    if you pass by value, the computation of the value will 
  //    be performed eagerly, and so it will not be parallelizable. Passing
  //    by name allows computation to be deferred until we actually call "run".
  // 
  
  // Exercise 2 (CB7.4)

  def asyncF[A,B] (f: A => B) : A => Par[B] = a => lazyUnit(f(a))


  // Exercise 3
  //
  // Write the answer here in a comment.

  //    The first step in testing a function like map, is to define what
  //    properties the function should have. With 'map', we would expect the
  //    following properties:
  //    
  //    - mapping on a Par should return a Par
  //    - mapping using a function f: A => B should return a Par[B]
  //    - mapping should apply 'f' to the underlying value of the input Par,
  //      such that:
  //      Par(1).map(_ * 2) -> Par(2)
  //    - mapping should retain the laziness of the orginal Par, such that its value
  //      is not evaluated simply by mapping (not until we call "run", or something
  //      similar)
  //    
  //    From here on we can write unit tests to ensure our implementation of Par
  //    meets these criteria.



  // Exercise 4 (CB7.5)

  def sequence[A] (ps: List[Par[A]]): Par[List[A]] = ps.foldRight(lazyUnit(List()): Par[List[A]])((cur: Par[A], acc: Par[List[A]]) => map2(cur, acc)(((c, a) => c +: a))) 

  // Exercise 5 (CB7.6)

  // this is shown in the book:

  def parMap[A,B] (as: List[A]) (f: A => B): Par[List[B]] =
    sequence (as map (asyncF (f)))

  def parFilter[A] (as: List[A]) (f: A => Boolean): Par[List[A]] = as.map(x => lazyUnit(x)).foldRight(lazyUnit(List()): Par[List[A]])((cur: Par[A], acc: Par[List[A]]) => map2(cur, acc)(((c, a) => if(f(c)) c +: a else a))) 

  // Exercise 6

  def map3[A,B,C,D] (pa: Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D): Par[D] = map2(pa, map2(pb, pc)((b, c) => (b, c)))((a, t) => {
    val (b, c) = t
    f(a, b, c)
  })

  // shown in the book (adjusted for the non-blocking version)

   def equal[A] (e: ExecutorService) (p: Par[A], p2: Par[A]): Boolean = 
     p(e) == p2(e)

  // Exercise 7 (CB7.11)

  // original implementation
  def _choiceN[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] = es => {
    val _n = run(es)(n)
    choices(_n)(es)
  }

  // original implementation
  def _choice[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(n)(b => if(b == true) 0 else 1))(List(t, f))
  }

  // Exercise 8 (CB7.13)

  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa)
    choices(a)(es)
  }

  // implementations using 'chooser()' 
  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] = chooser(n)(x => choices(x))

  def choice[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = chooser(n)(b => if(b == true) t else f)

  // Exercise 9 (CB7.14)

  def join[A] (a : Par[Par[A]]) :Par[A] = es => run(es)(a)(es)

  // Exercise 11

  def wget (uris: String*): List[String] = {
    val es = Executors.newFixedThreadPool(4)
    run(es)(parMap(uris.toList)(uri => scala.io.Source.fromURL(uri)("ISO-8859-1").mkString))
  }
}

// Exercise 10
//
// ...

// was able to call .map directly on a Par[Int] from the console, but tests failed...
case class ParOps[A] (val par: Par.Par[A]) {
  def map[B] (f: A => B) = Par.map(par)(f)
}

object ParOps {
  implicit def parOps[A] (par: Par.Par[A]): ParOps[A] = {
    ParOps(par)
  }
}
