package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException
    case Cons(_, t) => t
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("Can't replace head of empty list!")
    case Cons(_, t) => Cons(h, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _ => l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on am empty list!")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product2(ds: List[Double]):Double = foldRight(ds, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, l) => l + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  def foldLeftViafoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((x, acc) => f(acc, x))

  def foldRightViafoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((acc, x) => f(x, acc))

  def main(args: Array[String]): Unit = {
    println("successful compilation!")
    println("List companion object -")
    println(length(List(1, 2, 3, 4, 5)))
  }
}
