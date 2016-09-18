// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._ // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecCawrSaba extends FlatSpec with Checkers {

  import Stream._

  val ones: Stream[Int] = cons(1, ones)

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A](la: List[A]): Stream[A] = la.foldRight(empty[A])(cons[A](_,
      _))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A](implicit arbA: Arbitrary[A]): Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty) }
      yield list2stream(la)

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n: Int) => cons(n, empty).headOption == Some(n) }) &&
      ("random" |:
        Prop.forAll { (s: Stream[Int]) => s.headOption != None })

  }

  it should "not force the tail of the Stream (03)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream2 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("headOption_no_evaluation" |:
      Prop.forAll { (s: Stream[Int], n: Int) => { Stream.from(1).
        tails.headOption; 
      true } })
  }

  behavior of "take"

  //For take:
  /*- take should not force any heads nor any tails of the Stream it manipulates
  - take(n) does not force (n+1)st head ever (even if we force all of take(n))
  - s.take(n).take(n) == s.take(n) for any Stream s and any n (idempotency)*/

  it should "not force any heads nor any tails of the Stream (04)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream3 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("take_no_evaluation" |:
      Prop.forAll { (s: Stream[Int], n: Int) => { s.map(x => x / 0).take(n); 
      true } })
  }

  it should "not force n+1 head ever(05)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream4 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("take_no_eval_n+1_head" |:
      Prop.forAll { (s: Stream[Int], n: Int) => { s.take(n).tails.headOption; 
      true } })
  }

  it should "s.take(n).take(n) should be equal to s.take(n)(06)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream5 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("take_idempotency" |:
      Prop.forAll { (s: Stream[Int]) => s.take(4).take(4).toList == s.take(4).
      toList })
  }

  // For drop:
  behavior of "drop"
  //- s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)
  it should "s.drop(n).drop(m) should be equal to s.drop(n+m)(07)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream6 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("drop_additivity" |:
      Prop.forAll { (s: Stream[Int]) => s.drop(2).drop(2) == s.drop(4) })
  }

  //- s.drop(n) does not force any of the dropped elements heads
  it should "s.drop(n) does not force any of the dropped heads(08)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream7 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("drop_no_forced_heads_in dropped" |:
      Prop.forAll { (s: Stream[Int], n: Int) => s.map(x => x / 0).drop(n); 
      true })
  }

  //We encountered some difficulty implementing this test in relation to finding 
  //building blocks that did not   //force before the final call toList. We have 
  //tried to provoke an error using arithmetic exception, integer overflow and 
  //??? operator. Overflow seems to be ignored by runtime and ??? is immediately 
  //evaluated to   //an 'implementation needed' error. When trying to create the 
  //requested test scenario of a 'bad' Stream concatenated with a 'good' Stream 
  //- it seems that both append and cons forces the Stream...
  it should "force any dropped heads - even if tail forced (09)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream8 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("drop_no_dropped_forced_even_if_tail_forced" |:
      Prop.forAll { (s: Stream[Int], n: Int) => { s.take(n).map(x => x / 0).
        append(Stream(List(1, 2, 3))).drop(n).toList }; false })
  }

  behavior of "map"
  //For map:
  //- x.map(id) == x (where id is the identity function)
  //- map terminates on infinite streams

  it should "map x to identity (10)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream9 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("map_to_identity" |:
      Prop.forAll { (s: Stream[Int]) => s.map(x => x).toList == s.toList })
  }

  //We were not completely sure about the meaning of this point in specification
  it should "terminate on infinite stream (11)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream10 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("map_terminates_infinite" |:
      Prop.forAll { (n: Int) => Stream.from(1).map(x => x / 0); true })

  }

  behavior of "append"
  //For append:
  //- sum of individual streams should be sum of appended stream
  //- individual streams should be subsequences of appended stream
  //- length of individual streams should be length of appended stream
  //- streams should be appended in appropriate order

  it should "return appended stream with sum of indiv. streams (12)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream11 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("append_should_return_sum_of_payload" |:
      Prop.forAll { (s1: Stream[Int], s2: Stream[Int]) =>
        s1.append(s1).foldRight(0)((x, acc) => x +
          acc) == s1.foldRight(0)((x, acc) => x + acc) + 
          s2.foldRight(0)((x, acc) => x + acc); true
      })

  }

  it should "return append stream with indiv. streams subseqs (13)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream12 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("append_should_return_subsequences" |:
      Prop.forAll { (s1: Stream[Int], s2: Stream[Int]) =>
        s1.append(s2).hasSubsequence(s1) &&
          s1.append(s2).hasSubsequence(s2); true
      })

  }

  it should "return append stream length = sum of indiv. stream (14)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream13 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("append_should_return_sum_of_length" |:
      Prop.forAll { (s1: Stream[Int], s2: Stream[Int]) =>
        s1.append(s2).toList.size ==
          s1.toList.size + s2.toList.size; true
      })

  }

  it should "append streams in appropriate order (15)" in check {
    // the implicit makes the generator available in the context
    implicit def arbIntStream14 = Arbitrary[Stream[Int]](genNonEmptyStream[Int])
    ("append_should_return_order" |:
      Prop.forAll { (s1: Stream[Int], s2: Stream[Int], s3: Stream[Int]) => 
        s1.append(s2).append(s3).startsWith(s1); true })

  }

}
