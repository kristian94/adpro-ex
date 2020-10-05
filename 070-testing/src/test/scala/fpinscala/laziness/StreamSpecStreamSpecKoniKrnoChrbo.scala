// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

// If you  comment out all the  import lines below, then  you test the
// Scala  Standard Library  implementation of  Streams. Interestingly,
// the standard library streams are stricter than those from the book,
// so some laziness tests fail on them.

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecKoniKrnoChrbo
    extends org.scalatest.freespec.AnyFreeSpec 
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  import Stream._

  // A simple converter of lists to streams
  def list2stream[A] (la :List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // There  is  a name  clash  between  Stream.empty and  the  testing
  // library, so we need to qualify Stream.empty

  // An example generator  of random finite non-empty  streams (we use
  // the  built in  generator of  lists and  convert them  to streams,
  // using the above converter)
  //
  // 'suchThat'  filters  out  the  generated instances  that  do  not
  // satisfy the predicate given in the right argument.
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  def genNonNegativeInt (implicit arbA :Arbitrary[Int]) :Gen[Int] = 
    for (
      n <- Gen.choose(0, Integer.MAX_VALUE / 2 - 1)
    ) yield n

  "headOption" - {

    // a scenario test:

    "returns None on an empty Stream (01)" in {
      (Stream.empty.headOption) shouldBe (None)
    }


    // two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {
      forAll { (n :Int) => cons (n, Stream.empty).headOption should be (Some (n)) }
    }

    "returns the head of random stream packaged in Some (02)" in {
      // The implict makes the generator available in the context
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // This property uses our generator of non empty streams thanks to the
      // above implicit declaration
      forAll { (s :Stream[Int]) => s.headOption shouldNot be (None) }
    }

    "headOption should not force the tail of the stream" in {
      val s = cons(1, ???)
      s.headOption
    }
  }

  "take" - {
    "should not force any heads nor any tails of the Stream it manipulates" in {
      val s = cons(???, ???)
      s.take(2)
    }

    "take (n) does not force (n+1)st head ever (even if we force all elements of take(n))" in {
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s :Stream[Int]) => {
        val n = s.toList.size
        val s1 = s.append(cons(???, ???)) // if ??? is forced it would throw an exeption
        s1.take(n).toList
      } }
    }

    "s.take(n).take(n) == s.take(n) for any Stream s and any n (idempotency)" in {
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s :Stream[Int], n: Int) => {
        s.take(n).take(n).toList shouldBe s.take(n).toList
      } }
    }
  }

  "drop" - {
    "s.drop(n).drop(m) == s.drop(n + m) for any n, m (additivity)" in {
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      implicit def arbInt = Arbitrary[Int] (genNonNegativeInt)
      
      forAll { (s :Stream[Int], n: Int, m: Int) => {
        s.drop(n).drop(m).toList shouldEqual s.drop(n + m).toList
      } }
    }

    "s.drop (n) does not force any of the dropped elements heads" in {
      val s = cons(???, cons(1, Stream.empty))
      
      s.drop(1).toList
    }
    // The above should hold even if we force some stuff in the tail

  }

  "map" - {
    "x.map (id) == x (where id is the identity function)" in {
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      
      forAll { (s :Stream[Int]) => {
        s.map( identity ).toList == s.toList
      } }
    }

    val naturals: Stream[Int] = from(1)

    "'map' terminates on infinite streams" in {
      naturals.map(_ * 2)
    }
  }

  "append" - {
    "appending two empty streams should produce an empty stream" in {
      Stream.empty.append(Stream.empty).toList shouldEqual Stream.empty.toList
    }

    "appending a stream of length m, to a stream of length n, produces a stream of length n + m" in {
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
      
      forAll { (s1 :Stream[Int], s2 :Stream[Int]) => {
        val n = s1.toList.size
        val m = s2.toList.size

        val l = s1.append(s2).toList.size

        l shouldEqual n + m
      } }
    }

    "appending two infinite streams should terminate" in {
      val naturals: Stream[Int] = from(1)

      naturals.append(naturals)
    }

    "appending to a stream does not change its head" in {
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      forAll { (s1 :Stream[Int], s2 :Stream[Int]) => {
        val appended = s1.append(s2)

        s1.headOption shouldEqual appended.headOption 
      } }
    }
  }
}
