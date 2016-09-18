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

// An ADT of Lists


	//Exercise 1
	//First case will not match since '2' is not preceeded by '4' in the list
	//Second case will not match since list is not empty...
	//We think, that the match will happen in the third case, since variables x and y will match '1' and '2' in the list.
	//The match will return 3, which is the sum of the x and y variables in the list...
	//The fourth and the fifth case is also a match, but it is always the first match found, which is used... 	

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Test extends App {
    //println(List.drop(List(3,4,5,6,7),1))
    //println(List.dropWhile(List(3,4,5,6,7),{(x :Int)=>x<0}))
    //println(List.init(List(1,2,3,4,5)))
    //println(Exercise7.maximumSalary(Exercise7.test_case))
    //println(List.concat(List(List(3,4,5,6,7),List(13,14,15,16,17),List(23,24,25,26,27)))) 
    //println(List.filter(List(2,3,14,5,16,7))({ (x) => (x % 2) == 0 }))
    //println(List.flatMap(List(1,2,3)) (i =>List(i,i)))
    //println(List.add(List(1,6,3,41,5)) (List(7,6,15,4)))
    //println(List.length(List(1,4,5,2,3,7,8)))
    //println(List.sum(List(1,2,3,4,5,6)))
    //println(List.product(List(1,2,3,4,5,6)))
    //println(List.length(List(1,2,3,4,5,6)))
    //println(List.reverse(List(1,2,3,4,5,6)))
    //println(List.filter1(List(2,3,14,5,16,7))({ (x) => x < 10 }))
    //println(List.zipWith({ (x:Int, y:Int) => x + y }) (List(1,6,3,41,5), List(7,6,15,4)))
    println(List.hasSubsequence(List(1,2,3,4), List(1,2,3,4,5)))
    //println(List.pascal(7))
}

//TO DO: 12

object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function  
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
		case Nil => Nil
		case Cons(_,t) => t	
 }
 
  // Exercise 3 Implement the function setHead replacing the first element of a List with a different value

  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
		case Nil => Nil
		case Cons(h,t) => Cons(newHead, t)	
 }
   
  // Exercise 4
  def drop[A] (l: List[A], n: Int) : List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => { if(n>0) drop(tail(l),n-1) else l }
    
  }

  // Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => { if(f(h)) dropWhile(tail(l),f) else l }
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] = l match {
   case Nil => Nil
   case Cons(h,Cons(_,Nil)) => Cons(h,Nil)
   case Cons(h,t) => Cons(h,init(t))
  }

  // Exercise 7 is in the bottom of the file

  // Exercise 8
  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int = {
   List.foldRight(as,0) { (m: A, n:Int) => n + 1 }
  }

  // Exercise 9
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Cons (x, xs) => foldLeft(xs, f(z,x))(f)
    case Nil => z
  }

  // Exercise 10
  def sum (as : List[Int]): Int = { 
    List.foldLeft(as, 0) ({ (x, y) => x + y} )
  }
  def product (as :List[Int]) : Int = {
    List.foldLeft(as, 1) ( { (x, y) => x * y} )
  }
  def length1 (as :List[Int]) : Int = {
    List.foldLeft(as, 0) ( { (x, y) => x + 1 } )
  }

  // Exercise 11
  def reverse[A] (list :List[A]) :List[A] = {
    def turn[A] (result: List[A], original: List[A]): List[A] = original match {
      case Nil => result
      case Cons(h, t) => { turn(Cons(h, result), t ) }
    }   
    turn(Nil, list)  
  }
  
  // Exercise 12
  /*def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = {
    
  }

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    
  }*/

  // Exercise 13

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => append(h, concat(t))
  }
  
  // Exercise 14
  def map[A,B] (a :List[A]) (f :A => B) :List[B] = a match { 
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }
  
  // Exercise 15 (no coding) 
  // foldLeft traverses the list from head to the last element and foldRight traverses it from 
  // the last element to head. In order to obtain same order as the input list we use foldRight. 
  // Otherwise we would have to call reverse as in exercise 14 which gives overhead and not in
  // constant time.

  // Exercise 16
  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => { if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f) }
  }

  // Exercise 17
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => { append(f(h), flatMap(t)(f)) } 
  }
  
  // Exercise 18
  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = l match {
    case Nil => Nil 
    case Cons(h, t) => flatMap(l)( h => { if (p(h)) { List(h) } else { Nil } } )  
  }

  // Exercise 19
  def add (l: List[Int]) (r: List[Int]): List[Int] = (l, r) match {
    case (l,r) if (l == Nil || r == Nil) =>  Nil
    case (Cons(h1, t1),Cons(h2, t2)) => { Cons((h1+h2), add(t1)(t2)) }
  }
  
  // Exercise 20
  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l, r) match {
    case (l,r) if (l == Nil || r == Nil) =>  Nil
    case (Cons(h1, t1),Cons(h2, t2)) => { Cons(f(h1,h2), zipWith(f)(t1, t2))}
  }

  // Exercise 21
  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = (sup, sub) match {
    case (sup, sub) if (sup == Nil || sub == Nil) =>  false
    case (Cons(h1, t1), Cons(h2, t2)) => { if (subseqFound(sup, sub) (0)) { true } else { hasSubsequence(t1,sub) } }
  }
  
  def subseqFound[A] (sup1: List[A], sub1: List[A]) (count: Int) :Boolean = (sup1, sub1) match {
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => subseqFound(t1, t2) (count + 1)  
    case (sup, sub) => { if ((count >= 1) && (sub==Nil)) { true } else { false } } 
  }

  // Exercise 22
  def pascal (n :Int) : List[Int] = {
    //hard code case n==1 or 2 - delegate rest to helper 
    if (n == 1) { List(1) }
    else if (n == 2) { List(1,1) }
    else { 
      helper(n) (List(1,1))
    }
  }
  
  def helper (n: Int) (x: List[Int]) : List[Int] = {
    //only count down to 2 
    if (n > 2) {
      //add liste to itself with prefix 0 - append closing 1 and empty list
      helper((n-1))(append(add(x) (Cons(0, x)), Cons(1,Nil)))
    } else {
    //return result  
    x 
    }
  }
 
  //a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}




// Exercise 7

object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  def maximumSalary (salaries: List[SalaryLine]) :Integer = salaries match {
    case Nil => -1
    case Cons(h, t) => math.max(h.amount, maximumSalary(t)) //if (h.amount > maximumSalary(t)) h.amount else { maximumSalary(t) }
  }

  val test_case = List( SalaryLine("John",49),
    SalaryLine("Alice", 42),
    SalaryLine("Bob",40), SalaryLine("Jens",45))

}


