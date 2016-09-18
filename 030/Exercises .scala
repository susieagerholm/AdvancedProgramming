

import java.awt.Point


import scala.collection.immutable.TreeSet


/*
trait A { def x = 1 }
trait B extends A { override def x = super.x * 5 }
trait C1 extends B { override def x = 2 }
trait C2 extends A { this: B => override def x = 2}

// 1.
println((new C1 with B).x) // 2
println((new C2 with B).x) // 10

 * */





class OrderedPoint(x :Int, y :Int) extends Point(x,y) with scala.math.Ordered[Point] {
  
// this = OrderedPoint
// that = Point
  
override def compare(that: Point): Int = {
    if (this.x != that.y) (this.x - that.x)
    else {
      if (this.y != that.y) (this.y - that.y)
      else 0
    }
} // end override


} // end trait

// Chapter 3

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2 (3.25)

  def size[A] (t :Tree[A]) :Int = t match {
   case Leaf(_) => 1
   case Branch(l,r) => 1 + size(l) + size(r)  
  }

  // Exercise 3 (3.26)

  def maximum (t: Tree[Int]) :Int = {
   def maxUtil (t: Tree[Int], max :Int) :Int = t match {
    case Branch(l,r) => maxUtil(l,max); maxUtil(r,max) 
    case Leaf(x) => x.max(max)
   } 
   maxUtil(t,0)
     
  }
  
 
  // Exercise 4 (3.27)

  def depth[A](t: Tree[A]): Int = t match {
   case Leaf(_) => 0
   case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
   
   
   /* Just to get an understanding of depth */
 def depthDisplay[A](t: Tree[A]): Int = t match {
  case Leaf(_) =>
    print("0")
    0
  case Branch(l, r) =>
    print("1 + (")
    val d1 = depth(l)
    print(d1 + " max ")
    val d2 = depth(r)
    print(d2 + ")")
    println()
    1 + (d1 max d2)
}
    

  // Exercise 5 (3.28)

  def map[A,B] (t: Tree[A]) (f: A => B) : Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch( map(l)(f), map(r)(f))
  }
  

  // Exercise 6 (3.29)

  def fold[A,B] (t: Tree[A]) (g: A => B) (f: (B,B) => B) :B = t match {
   case Leaf(x) => g(x)   
   case Branch(l,r) => f(fold(l)(g)(f),fold(r)(g)(f))  
  }
    
 
  def size1[A] (t: Tree[A]) :Int = {
     fold(t) ({n  => 1}) ({(x :Int, y :Int) => 1 + x + y }) 
  }

  def  maximum1 (t: Tree[Int]) :Int = {
     fold(t) ({n  => n }) ({(x :Int, y :Int) => x.max(y) }) 
  }
    
  def depth1[A] (t: Tree[A]) :Int =   {
    fold(t) ( {n => 0} ) ({ (x :Int, y :Int) => 1+ x.max(y) })
  }

  
  def map1[A,B] (t: Tree[A]) (f: A => B) : Tree[B] =  {
    fold(t) (a => Leaf(f(a)) :Tree[B]) ({ (x, y) => Branch(x,y) })
               // Cast to Tree 
  }
  
  
}

   
sealed trait Option[+A] {

    
  // Exercise 7 (4.1)

  def map[B] (f: A=>B) : Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  // Ignore the arrow in default's type this week
  // (it should work (almost) as if it was not there)

  
  def getOrElse[B >: A] (default: => B) :B = this match {
    case None => default 
    case Some(x) => x 
    
  }
  
  def flatMap[B] (f: A=>Option[B]) : Option[B] = this match {
    case None => None 
    case Some(x) => f(x)
  }
    
  // Ignore the arrow in ob's type this week

  def orElse[B >: A] (ob : => Option[B]) : Option[B] = this match {
    case None => ob 
    case Some(x) => Some(x)
  }

  def filter (f: A => Boolean) : Option[A] = this match {
    case None => None 
    case Some(x) => if(f(x)) Some(x) else None
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]


object ExercisesOption {

  // Remember that mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 8 (4.2)
   // Getting the value from an Option: val x = toInt("1").getOrElse(0)
   def variance (xs: Seq[Double]) : Option[Double] = {
      /* Don't quite understand why it is necessary to call mean on the result of the map. 
         Is variance defined as the average of the map computation?
      */
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

  // Exercise 9 (4.3)

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C) :Option[C] = {
    if(Option(ao)==None || Option(bo)==None) None
    else ao.flatMap(aof => bo.map(bof => f(aof, bof)))
  }

  // Exercise 10 (4.4)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(p => sequence(t).map(p :: _))
  }
  

  // Exercise 11 (4.5)

  def traverse[A,B] (as: List[A]) (f :A => Option[B]) :Option[List[B]] =  {
       //Some(Nil) is start value - creating a List from right 
       as.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _)) 
  }
  
}



// Test cases for running in the compiled vesion (uncomment as you go, or paste
// them into REPL in the interactive version)

object Tests extends App {

  
  // Exercise 1
//   val p = new java.awt.Point(0,1) with OrderedPoint
//   val q = new java.awt.Point(0,2) with OrderedPoint
//   assert(p < q)

  // Notice how we are using nice infix comparison on java.awt
  // objects that were implemented way before Scala existed :) (And without the
  // library implementing a suitable comparator). We did not have to recompile
  // java.awt.Point


  // Exercise 2
  //assert (Tree.size (Branch(Leaf(1), Leaf(2))) == 3)
  // Exercise 3
  // assert (Tree.maximum (Branch(Leaf(1), Leaf(2))) == 2)
  //println(Tree.maximum (Branch(Leaf(1), Leaf(3))))
  //val tree1 = Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
  //println(Tree.maximum(tree1))
  
  // Exercise 4
  // val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
   //assert (Tree.depth (t4) == 3)
  // val test = Branch(Leaf(2),Branch(Leaf(4),Leaf(6)))
  // println ("Result: " + Tree.depth2 (t4))
   
  // Exercise 5
  // val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"),Leaf("3")),Leaf("4")))
  //println (Tree.map (t5) (_.toString))
  // assert (Tree.map (t5) (_.toString) == t5)

   
  // Exercise 6
 // assert (Tree.size1 (Branch(Leaf(1), Leaf(2))) == 3)
 // val tree1 = Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
 // println (Tree.size1 (tree1))

  
  //assert (Tree.maximum1 (Branch(Leaf(1), Leaf(2))) == 2)
  // assert (Tree.depth1 (t4) == 3)
  // assert (Tree.map1 (t4) (_.toString) == t5)

  // Exercise 7
  // assert (Some(1).map (x => x +1) == Some(2))
  // assert (Some(42).getOrElse(41) == 41)
 //  println(Some(41).getOrElse(42))

  // assert (None.getOrElse(42) == 42)
  // assert (Some(1).flatMap (x => Some(x+1)) == Some(2))
  // assert ((None: Option[Int]).flatMap[Int] (x => Some(x+1)) == None)
  // assert (Some(41).orElse (Some(42)) == Some(41))
  // assert (None.orElse (Some(42)) == Some(42))
  // assert (Some(42).filter(_ == 42) == Some(42))
  // assert (Some(41).filter(_ == 42) == None)
  // assert ((None: Option[Int]).filter(_ == 42) == None)

  // Exercise 8
  //  ExercisesOption.variance (List(42,42,42))
  // assert (ExercisesOption.variance (List(42,42,42)) == Some(0.0))
  // assert (ExercisesOption.variance (List()) == None)


  // Exercise 9
  // assert (ExercisesOption.map2 (Some(42),Some(7)) (_ + _) == Some(49))
  // assert (ExercisesOption.map2 (Some(42),None) (_ + _) == None)
  // assert (ExercisesOption.map2 (None: Option[Int],Some(7)) (_ + _) == None)
  // assert (ExercisesOption.map2 (None: Option[Int],None) (_ + _) == None)

  // Exercise 10
  // assert (ExercisesOption.sequence (List(Some(1), Some(2), Some(42))) == Some(List(1,2,42)))
  // assert (ExercisesOption.sequence (List(None,    Some(2), Some(42))) == None)
  // assert (ExercisesOption.sequence (List(Some(1), None,    Some(42))) == None)
  // assert (ExercisesOption.sequence (List(Some(1), Some(2), None    )) == None)

  // Exercise 11
  // def f (n: Int) :Option[Int] = if (n%2 == 0) Some(n) else None
  // assert (ExercisesOption.traverse (List(1,2,42)) (Some(_)) == Some(List(1,2,42)))
  // assert (ExercisesOption.traverse (List(1,2,42)) (f) == None)

}
