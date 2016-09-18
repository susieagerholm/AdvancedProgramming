import scala.annotation.tailrec


// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.

object Exercises extends App {

  // Exercise 3
  
 def power(x: Double, n: Int) : Double = {
	if( n == 0 ) { 1 }
	else if (n > 0 && n % 2 == 0) { power(x, n/2) * power(x, n/2) }
	else if (n > 0 && n % 2 != 0) { x * power(x, n-1) }
	else { 1 / power(x, -n) }	
  }

  // A few tests, uncomment when your implementation is ready.

  assert (power (2.0, 2) == 4.0)
  assert (power (1.0, 42) == 1.0)
  //
  // The above assertions should pass when you call "scala Exercises".
  //
  // The following one should fail. Uncomment to check that assert works as
  // expected:
  //
  //assert (power (1.0, 42) == 2.0)

  // add 2-3 more tests:
  //
  println(power(5.0,2))
  println(power(5.0,5))
  println(power(5.0,-2))

  // Exercise 4

  def fib (n: Int) : Int = {
	if(n <= 0) { 0 }
	else if (n == 1) { 1 }
	else { fib(n-2)+fib(n-1) }
  }
  
  // some tests (uncomment, add more):
  assert (fib (0) == 0)
  assert (fib (1) == 1)
  assert (fib (2) == 1)
  assert (fib (3) == 2)
  assert (fib (4) == 3)
  assert (fib (5) == 5)
  assert (fib (6) == 8)
  assert (fib (7) == 13)
  
  println (fib (0))
  println (fib (1))
  println (fib (2))
  println (fib (3))
  println (fib (4))
  println (fib (5))
  println (fib (6))
  println (fib (7))


  // Exercise 5

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
   def this (tag :String, price :Int) = {
      this()
      this.tag = tag
      this.price = price
  }

    var tag   :String = "" // a tag line in the accounting system
    var price :Int    = 0 // the price is in cents
  }

  // computes the total of expenses in cents
  def total (expenses: Array[Expense]) :Int = {
	expenses.map(_.price).reduce {_+_}
  }

   val testcase1 = Array[Expense](
   new Expense("Coffee", 450),
   new Expense("Cake", 350) )
 
   val testcase2 = Array[Expense](
   new Expense("Coffee", 100),
   new Expense("Coffee", 100),
   new Expense("Cake", 0) ) 	
   
   val testcase3 = Array[Expense](
   new Expense("Coffee", 50),
   new Expense("Cake", -50))
   

  // Add one or two more tests
   assert (total (testcase1) == 800) // uncomment
   assert (total (testcase2) == 200) // uncomment
   assert (total (testcase3) == 0) // uncomment


  // Exercise 6
  
	def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = {
		as.toSeq.sliding(2).forall { case Seq(x, y) => ordered(x, y) }
	}

  // some tests (uncomment)

  assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))
  
  println ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  println (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  println (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))

  // add two tests with another type, for example an Array[String]
  println("Strings...")
  println ( isSorted (Array("a","b","c","d","e","f"), (a: String, b: String)=> a <= b))
  println (!isSorted (Array("f","b","c","d","e","f"), (a: String, b: String)=> a <= b))
  println ( isSorted (Array("f","b","c","d","e","f"), (a: String, b: String)=> a <= b))

  // Exercise 7: a curried version of solution to exercise 3

  def power1(x: Double) (n: Int) :Double = {
	if( n == 0 ) { 1 }
	else if (n > 0 && n % 2 == 0) { power(x, n/2) * power(x, n/2) }
	else if (n > 0 && n % 2 != 0) { x * power(x, n-1) }
	else { 1 / power(x, -n) }	
  }
  //def add(x:Int) = (y:Int) => x + y
  println("TRY POWER1:")
  println(power1(5.0)(2))

  // Exercise 8

  def curry[A,B,C] (f: (A,B)=>C) : A => (B => C) = { (a: A) => { (b: B) => f(a,b) } }
  
  //test if it type checks by currying power automatically:

  val power_curried: Double => Int => Double = curry(power)
  println("TRYING OUT CURRIED POWER...")
  println(power_curried(4.0)(3))

  // Exercise 9

  def uncurry[A,B,C] (f: A => B => C) : (A,B) => C = { (a: A, b: B) => f(a)(b) }

  val power_uncurried: (Double,Int) => Double = uncurry(power_curried)
  println("TRYING OUT UNCURRIED POWER...")
  println(power_uncurried(4.0, 4))

  // Exercise 10
  
  def compose[A,B,C] (f: B => C, g: A => B) : A => C = {
	a:A => f.apply(g.apply(a))
  }
  
  println (compose({m: Int => m+1}, {n: Int => n+2})(10))
  
  //how to make call to compose when composite functions are defined externally??? 
  //assert ((compose({m: Int => m+1}, {n: Int => n+2})(10)), 13)
  

}
