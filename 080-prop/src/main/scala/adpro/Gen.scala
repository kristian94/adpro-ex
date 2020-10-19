// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
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
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.
//
// Before starting to work on the exercises, familiarize yourself with the the
// content already in the file (it has been explained in chapter 8, but it is
// useful to see it all together in one file).

package adpro

import fpinscala.laziness.Stream._
import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.RNG._

object WarmupExercises {


  // Exercise 1
  lazy val rng1: RNG = RNG.Simple (42)

  // Exercise 2
  lazy val x: Int = ???
  lazy val y: Int = ???

  // Exercise 3
  lazy val s_random_int: State[RNG,Int] = ???
  lazy val s_nonNegativeInt: State[RNG,Int] = ???
  lazy val s_double: State[RNG,Double] = ???

  lazy val random_int: Int =  ???
  lazy val nonNegativeInt: Int =  ???
  lazy val double: Double = ???

  import Gen.state2stream

  // Exercise 4
  def randomDoubles (seed: RNG): Stream[Double] = ???

  lazy val someRandomDoubles: List[Double] = ???
  lazy val moreRandomDoubles: List[Double] = ???

  // Exercise 5
  def impureRandomDoubles: Stream[Double] = ???

  lazy val impureDoubles1: Stream[Double] = ???
  lazy val impureDoubles2: Stream[Double] = ???

}

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other staff)
case class Gen[A] (sample: State[RNG,A]) {

  // Let's convert generator to streams of generators
  def toStream (seed: Long): Stream[A] =
    Gen.state2stream (this.sample) (RNG.Simple (seed))

  def toStream (rng: RNG): Stream[A] =
    Gen.state2stream (this.sample) (rng)

  // Exercise 8

  def listOfN (n: Int): Gen[List[A]] = ???

  // Exercise 9

  def flatMap[B] (f: A => Gen[B]): Gen[B] =
    Gen (sample flatMap[B] (f (_).sample))

  // It would be convenient to also have map  (uses flatMap)

  def map[B] (f: A => B): Gen[B] = this.flatMap (a => Gen.unit[B] (f(a)))

  // Exercise 10

  def listOfN (size: Gen[Int]): Gen[List[A]] = ???

  // Exercise 11

  def union (that: Gen[A]): Gen[A] = ???

  // Exercise 12 continues in the companion object (below)
}

object Gen {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private[adpro]  def state2stream[A] (s :State[RNG,A]) (seed :RNG) :Stream[A] =
    s.run(seed) match { case (n,s1) => cons (n, state2stream (s) (s1)) }

  // A generator for Integer instances

  def anyInteger: Gen[Int] = Gen(State(_.nextInt))

  // Exercise 6

  def choose (start: Int, stopExclusive: Int): Gen[Int] = ???

  // Exercise 7

  def unit[A] (a: =>A): Gen[A] = ???

  def boolean: Gen[Boolean] = ???

  def double: Gen[Double] = ???

  // (Exercise 8 is found in the Gen class above)

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified (
    failure: FailedCase,
    successes: SuccessCount
  ) extends Result {
      def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A] (as: Gen[A]) (f: A => Boolean): Prop = Prop {

    (n,rng) => as.toStream (rng).zip (Stream.from(0)).take(n).map {

      case (a,i) => try {
        if (f (a)) Passed else Falsified (a.toString, i)
      } catch { case e: Exception => Falsified (buildMsg(a, e), i) }

    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A] (s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._

case class Prop (run: (TestCases,RNG)=>Result) {

  // (Exercise 12)

  def && (that: Prop): Prop = ???

  def || (that: Prop): Prop = ???

}

// vim:cc=80:foldmethod=indent:nofoldenable
