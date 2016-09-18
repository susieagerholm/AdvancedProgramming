// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monoids
import scala.language.higherKinds

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // I guess such objects could be useful if we combine them with implicits

  def listMonoid[A] = new Monoid[List[A]] {
    def op (a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 10.1

  val intAddition = new Monoid[Int] {
    def op (a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }
  
  val intMultiplication = new Monoid[Int] {
    def op (a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }  
  
  val booleanOr = new Monoid[Boolean] {
    def op (a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false //false leaves choice up to value of x
  }
  
  val booleanAnd = new Monoid[Boolean] {
    def op (a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true //true leaves choice up to value of x
  }

  // Exercise 10.2

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op (a1: Option[A], a2: Option[A]) = a1 orElse a2  //associative function, that combines Option a1 and a2 into one!! = orElse
    val zero = None
  }


  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op (f: A => A, g: A => A) = f andThen g //associative function, that combines Function f and g into one!! = andThen
    val zero = (a:A) => a  
  }

  // Exercise 10.4 is solved in MonoidSpec.scala
  //
  // I am using scalacheck to test my monoids (not our own framework)
  // run the tests using "sbt test"
  // Look into the file to see how to write property tests in scalacheck.
  // Use that file as an example template for your own tests

  def concatenate[A] (as: List[A], m: Monoid[A]): A =
    as.foldLeft (m.zero) (m.op)

  // Exercise 10.5 (easy)
  def foldMap[A,B] (as: List[A], m: Monoid[B]) (f: A=>B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))    

  // Exercise 10.7

  def foldMapV[A,B] (v: IndexedSeq[A], m: Monoid[B]) (f: A => B) :B =
    if (v.length == 0) m.zero
    else if (v.length == 1) f(v.head)
    else 
      m.op(foldMapV(v.splitAt(v.length / 2)._1, m)(f), 
        foldMapV(v.splitAt(v.length / 2)._2, m)(f)) 
              

  // Exercise 9
  //
  // Implement a productMonoid that builds a monoid out of two monoids. Test it
  // with scala check for instance by composing an Option[Int] monoid with a
  // List[String] monoid and running through our monoid laws.

  def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) =
     new Monoid[(A, B)] {                                //create new monoid from two separate...
      def op(x: (A, B), y: (A, B)) =
        (ma.op(x._1, y._1), mb.op(x._2, y._2))
      val zero = (ma.zero, mb.zero)
    } 

  // Exercise 10.17

  // def functionMonoid[A,B] (mb: Monoid[B]) =

}


trait Foldable[F[_]] {

  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
  def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 10.15 (we use the mixed nature of traits here)

  def toList[A] (fa: F[A]) :List[A] =
    foldLeft (fa) (List[A]()) ((a,b) => b :: a)
}

// Exercise 10.12 We just do Foldable[List]

object Foldable extends Foldable[List] {

  def foldRight[A,B] (as: List[A]) (b: B) (f: (A,B) => B): B = as match {
    case Nil => b
    case a::as1 => f(a, foldRight (as1) (b) (f))
  }

  def foldLeft[A,B] (as: List[A]) (b: B) (f: (B,A) => B): B = as match {
    case Nil => b
    case a::as1 => foldLeft (as1) (f(b,a)) (f)
  }

  // using foldLeft would not be a good idea for Streams (there I would use
  // foldRight)
  def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B =
    foldLeft (as) (mb.zero) ((b,a) => mb.op(b,f(a)))

}

/*object ListFoldable extends Foldable[List] {
   override def foldLeft[A,B] (as: List[A]) (z: B) (f: (B,A) => B): B = {
     as.foldLeft(z)(f)
   }
   ///see reference solution
       
}

object OptionFoldable extends Foldable[Option] {
   override def foldLeft[A,B] (as: Option[A]) (b: B) (f: (B,A) => B): B = as match {
    case None => b
    case Some(cc) => f(b, cc)
   }   
}*/
// Exercise 10.14

object FoldableOption extends Foldable[Option] {

  def foldRight[A,B] (a: Option[A]) (b: B) (f: (A,B) => B): B = a match {
    case None => b
    case Some (a) => f(a,b)
  }

  def foldLeft[A,B] (a: Option[A]) (b: B) (f: (B,A) => B): B = a match {
    case None => b
    case Some (a) => f(b,a)
  }

  def foldMap[A,B] (a: Option[A]) (f: A => B) (mb: Monoid[B]): B =
    foldLeft (a) (mb.zero) ((b,a) => mb.op(b,f(a)))
}



// vim:cc=80:tw=80
