package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(l)(from => Cons(mapper(from), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1): from =>
        if (pred(from)) Cons(from, Nil()) else Nil()

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      @tailrec
      def _zip(l: Sequence[(A,B)])(first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2, t2))     => _zip(concat(l, Cons((h1, h2), Nil())))(t1, t2)
        case _                                => l

      _zip(Nil())(first, second)

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0   => Cons(h, take(t)(n - 1))
      case _                     => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h, t) => Cons(h, concat(t, l2))
      case _ => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _          => Nil()

    @tailrec
    def foldLeft[A, B](l: Sequence[A])(acc: B)(mapper: (A, B) => B): B = l match
      case Cons(h, t) => foldLeft(t)(mapper(h, acc))(mapper)
      case Nil()      => acc

    def min(l: Sequence[Int]): Optional[Int] =
      foldLeft(l)(Optional.Empty())((x, acc) => acc match
        case Optional.Empty() => Optional.Just(x)
        case Optional.Just(y) => Optional.Just(if (y < x) y else x)
      )
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
