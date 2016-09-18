// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

//package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
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

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption
  
  //exercise 2:
  def toList :List[A] = 
     this match {
        case Empty => List()
        case Cons(h,t) => h() :: t().toList
  }
  
  //exercise 3:
  def take (n :Int) :Stream[A] =
    this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 1) cons(h(), t().take(n-1)) else cons(h(), Empty)
    }
  
  def drop (n :Int) :Stream[A] =
    this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 1) t().drop(n-1) else t()
  }
  
  //Exercise 3: naturals.take(1000000000).drop(41).take(10).toList returns really fast, because
  //it is only the numbers needed to compute the final result, that are instantiated in memory - 
  //even if a very large number is mentioned in the first part of the computation... 
  
  //exercise 4:
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
  }
    
  //exercise 5: 
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }  
 
  def forAll2(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll2(p) else false 
  }  
  
  //exercise 6
  def takeWhile2(p: A => Boolean):  Stream[A] =
    foldRight[Stream[A]] (empty[A]) (( h,t ) =>  if (p(h)) cons(h,t) else empty)
    
  //exercise 7
  def headOption2() :Option[A] =
    foldRight[Option[A]] (None: Option[A]) (( h,_ ) => Some(h))
    
  //exercise 8 
  def map2[B](f: A => B): Stream[B] = 
    foldRight[Stream[B]] (empty[B]) (( h,t ) => cons(f(h), t))
  
  def filter2(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]] (empty[A]) (( h,t ) => if (f(h)) cons(h,t) else t)
  
  def append2[B>:A](s: => Stream[B]): Stream[B] =
    foldRight[Stream[B]] (s) (( h,t ) => cons(h,t))
  
  def flatMap2[B](f: A => Stream[B]): Stream[B] = 
    foldRight[Stream[B]] (empty[B]) (( h,t ) => f(h) append2(t))
  
  //exercise 9 
  //Stream would only be evaluated until finding the first member, that satisfies p - so a Stream
  //would be a more efficient implementation than list...   
    
  //exercise 11  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((h, t)) => cons(h, unfold(t)(f))
  }
  
  //exercise 13
  def map3[B](f: A => B): Stream[B] = 
    unfold(this) {
     case Cons(h,t) => Some((f(h()), t()))
     case _ => None
  } 
  
  def take3(n :Int) :Stream[A] = 
    unfold(this, n) {  
     case (Cons(h,t), 1) => Some((h(), (empty, 0)))
     case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1))) 
     case _ => None
  }
    
  def takeWhile3(p: A => Boolean):  Stream[A] =
    unfold(this) { 
     case Cons(h,t) if (p(h())) => Some((h(), t()))
     case _ => None
  }
  
  def zipWith3[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold(this, s2) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2()))) 
      case _ => None
    }
  }
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) { m => m match {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
  }
  }
  
  //exercise 14
  def startsWith[A](that: Stream[A]): Boolean = {
   zipAll(that).takeWhile(x => !x._1.isEmpty && !x._2.isEmpty).forAll(y => y._1 == y._2)     
  }
  
  //exercise 15
  def tails2: Stream[A] = this match {  //without unfold
    case Cons(h, t) => cons(h(),t()).append2(t().tails2)
    case _ => Empty 
  }
  
  /*def tails3:Stream[A] = //with unfold
    unfold[Stream[A], Stream[A]](this) { s => s match { 
      case _ => None  
      case Cons(h, t) => Some(cons(h(),t()).append2(t()), t()) //must be flattened, but how?
    }  
  }*/
  
 /* def tails: Stream[A] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append2 Stream(empty[A])*/
}




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
  
  def from (n :Int) :Stream[Int] = {
    cons(n, from(n+1))
  } 
  
  def to (n :Int) :Stream[Int] = {
    def go(n: Int, c:Int): Stream[Int] = {
     if (c > 0) {
       cons(((n - c) + 1), go(n, (c-1)))
     }
     else Empty
    }
    go(n, n)
  }
  
  //exercise 10    
  val fibs = {
    def fibonacci(x: Int, y: Int): Stream[Int] = 
      cons(x, fibonacci(y, x + y))
    fibonacci(0,1)
  }
   
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    cons(f(z).get._1, unfold(f(z).get._2)(f))
  }
  
  //exercise 12
  def from1(n: Int): Stream[Int] = {
    unfold(1)(i => Some(i + 1, (i + 1)))
  }
  
  def fibs1() :Stream[Int] = {
     unfold((0,1))(i => Some( i._1 ,   (i._2,(i._1 + i._2))    )   )
   }
  
    
}

case object Empty extends Stream[Nothing]
 case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Test extends App {
  
  val naturals = from(1)
  println(naturals)
  
  val naturals_bool = Stream(true, true, true)
  
  val naturals1 = to(25)
  println("TEST")
  println(naturals1.drop(3).toList)
  
  println(naturals.take(1000000000).drop(41).take(10).toList) 
  
  println(naturals.takeWhile(_<1000000000).drop(100).take(50).toList)
  
  println(naturals1.forAll2(_<23))
  
  println(naturals.takeWhile2(_<1000000000).drop(100).take(50).toList)
  
  println(naturals1.headOption2())
  
  println(naturals1.map2(x => x * 2).toList)
  
  println(naturals.map2(_*2).drop (30).take (50).toList)
  
  println(naturals1.filter2(x => x < 20).toList)
  
  println(naturals.drop(42).filter2(_%2 ==0).take(30).toList) 
  
  println(naturals.flatMap2(to _).take(100).toList)
  
  println(naturals.flatMap2(x =>from (x)).take (100).toList)
  
  println(unfold(2)(i => Some(i, i + 1)).take(10).toList)
  
  println(naturals.take(10).append2(naturals).take(20).toList)
  
  println(fibs.take(20).toList)
  
  //println(unfold(1)(x => x + 2))
  
  /*from(1).take(1000000000).drop (41).take(10).toList */
    
  println(from1(1).take(1000000000).drop (41).take(10).toList)
  println(fibs1.take(100).toList) //==fibs.take(100).toList, 
  
  println(naturals1.take3(6).map3(x => x * 2).toList)
  
  println(naturals1.takeWhile3(x => x < 12).toList)
  
  //println(naturals.zipWith[Int,Int](_+_)(naturals).take(2000000000).take(20).toList)
  
  println(naturals.zipAll(fibs).take(10).toList)
  
  //println(naturals.map2(_%2==0).zipWith[Boolean,Boolean] (_||_) (naturals.map2(_%2==1)).take(10).toList)
  
  //println(naturals.zipWith[Int,Int](_+_)(naturals).take(2000000000).take(20).toList)
  //naturals.map2(_%2==0).zipWith[Boolean,Boolean](naturals.map2(_%2==1)).take(10).toList
  
 println(naturals.map2[Boolean](_%2==0).zipWith3[Boolean,Boolean] (naturals.map2[Boolean](_%2==1)) (_||_).take(10).toList) 
  
  //println(naturals.map2(_%2==0).zipWith[Boolean,Boolean] (naturals_bool) (naturals.map2(_%2==1)).take(10).toList)
  
  println(naturals.startsWith(naturals.take(100)))
  println(naturals.startsWith(fibs.take(100)))
  
  
  
} 



// vim:tw=0:cc=80:nowrap
