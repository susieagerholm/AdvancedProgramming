import scala.annotation.tailrec 

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }
  
  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // exercise 1 (CB 6.1)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val rng2 = rng.nextInt
    if (rng2._1 >= 0) rng2
    else if (rng2._1 > Int.MinValue) (- rng2._1, rng2._2)      //invert negative to positive 
    else (Int.MaxValue, rng2._2)                               //cover corner case Int.MinValue
  }

  // exercise 2 (CB 6.2)
  def double(rng: RNG): (Double, RNG) = { 
    val(n, rng2) = nonNegativeInt(rng)
    if(n == 0) (n.toDouble, rng2) 
    else ((n / (Int.MaxValue.toDouble + 1.0).toDouble), rng2)
  }
  
  // exercise 3 (CB 6.3)
  def intDouble(rng: RNG): ((Int, Double), RNG) = { 
    var (i, rng2) = nonNegativeInt(rng)
    var (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = { 
    var (d, rng2) = double(rng)
    var (i, rng3) = nonNegativeInt(rng2)
    ((d, i), rng3)  
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = { 
    var (d1, rng2) = double(rng)
    var (d2, rng3) = double(rng2)
    var (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }
  
  // exercise 4 (CB 6.4)
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
     @tailrec def ints_state(count: Int)(rng: RNG)(l: List[Int]): (List[Int], RNG) = {
      var (i, rng2) = rng.nextInt
      if(count > 0) ints_state(count - 1)(rng2)(i :: l)
      else (l, rng2)
    }
    ints_state(count)(rng)(List())
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
    
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)
  val map_double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6 (CB 6.6)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>     //feeding into the Rand function: How???
      val(a,rng2) = ra(rng)
      val(b,rng3) = rb(rng2)  
    (f(a, b), rng3)
  }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight[Rand[List[A]]] ( unit(List[A]()) ) ((x, acc) => map2(x, acc)(_ :: _))
  }

  def seq_ints(count: Int): Rand[List[Int]] = {
    sequence[Int](List.fill(count)(int))
  }

  // Exercise 8 (6.8)
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val(a, rng2) = f(rng)
      g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { 
    flatMap(nonNegativeInt) {
      x => 
        val mod = x % n
        if (x + (n-1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
    }
  }

  // Exercise 9 (6.9)
  def flat_map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def flat_map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) (a => map(rb) (b => f(a,b)))
  }
}

import State._

case class State[S, +A](run: S => (A, S)) {            //exact purpose of run function??? unfold s (seed) to state???

  //exercise 10 (6.10)
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a))) 
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    flatMap(a => sb.map(b => f(a, b)))
   
    
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => { //I don't get this solution...
    val(a, s1) = run(s)                                            
    f(a).run(s1)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 10 (6.10) continued
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = 
    sas.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
  
  // This is given in the book:

  def modify[S](f: S => S): State[S, Unit] = for {
     s <- get // Gets the current state and assigns it to `s`.
     _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
   } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // exercise 11
  /*def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    How to start snowball of new states from combining state s and seed - 
    no obvious signature for building blocks? 
    
  }*/

  // Exercise 12

  // val random_integers = ...

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/*object Candy {
  

  // Exercise 13 (CB 6.11)
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
     
  }
}*/

// vim:cc=80:foldmethod=indent:foldenable

object Test extends App {
  
  val rng1 = RNG.Simple(45465656)
  val rng2 = RNG.Simple(45754742)
  
  //exercise 1
  println("exercise 1")
  println(RNG.nonNegativeInt(rng2)) 
  
  //exercise 2
  println("exercise 2")
  println(RNG.double(rng1))
  
  //exercise 3
  println("exercise 3")
  println(RNG.intDouble(rng1))
  println(RNG.doubleInt(rng1))
  println(RNG.double3(rng1))
  
  //exercise 4
  println("exercise 4")
  println(RNG.ints(8)(rng2))
  
  //exercise 5
  println("exercise 5")
  println(RNG.map_double(rng1)._1)
  
  //exercise 6
  println("exercise 6")
  val ra = RNG.nonNegativeEven
  val rb = RNG.nonNegativeEven
  val seed = ra(rng1)
  println(seed._1 + ", " + rb(seed._2)._1)
  println(RNG.map2(ra, rb)((x, y) => x + y)(rng1)._1)
  
  //exercise 7
  println("exercise 7")
  var a = RNG.nonNegativeEven
  var b = RNG.nonNegativeEven
  var c = RNG.nonNegativeEven
  val seed1 = a(rng1)
  val seed2 = b(seed1._2)
  println(seed1._1 + ", " + b(seed1._2)._1 + ", " + c(seed2._2)._1)
  var z = List(a, b, c)
  println(RNG.sequence(z)(rng1)._1)
  
  println(RNG.seq_ints(5)(rng1)._1)
  
  //exercise 8
  println("exercise 8")
  println(RNG.nonNegativeLessThan(Integer.MAX_VALUE)(rng1)._1)
  println(RNG.nonNegativeLessThan(12000)(rng1)._1)
  
  //exercise 9
  println("exercise 9")
  println(RNG.flat_map(RNG.map_double)(x => x * 2)(rng1)._1)
  //println(RNG.flat_map2(RNG.map_double, RNG.map_double)(x => x * 2)(rng1)._1)
  
  //exercise 10
  println("exercise 10")
    
  
}
