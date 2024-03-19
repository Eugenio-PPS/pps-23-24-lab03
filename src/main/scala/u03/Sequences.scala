package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension [A](l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

      def min: Optional[Int] =
            l.foldLeft(Optional.Empty())((x, acc) => acc match
              case Optional.Empty() => Optional.Just(x)
              case Optional.Just(y) => Optional.Just(if (y < x) y else x)
            )


    extension[A] (l: Sequence[A] )
      def map[B](mapper: A => B): Sequence[B] =
        l.flatMap(from => Cons(mapper(from), Nil()))

      def filter(pred: A => Boolean): Sequence[A] =
        l.flatMap: from =>
          if (pred(from)) Cons(from, Nil()) else Nil()

      // Lab 03
      def zip[B](second: Sequence[B]): Sequence[(A, B)] =
        @tailrec
        def _zip(l1: Sequence[(A,B)])(first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
          case (Cons(h1, t1), Cons(h2, t2))     => _zip(l1.concat(Cons((h1, h2), Nil())))(t1, t2)
          case _                                => l1

        _zip(Nil())(l, second)

      def take(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0   => Cons(h, t.take(n - 1))
        case _                     => Nil()

      def concat(l2: Sequence[A]): Sequence[A] = l match
        case Cons(h, t) => Cons(h, t.concat(l2))
        case _ => l2

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
        case _          => Nil()

      @tailrec
      def foldLeft[B](acc: B)(mapper: (A, B) => B): B = l match
        case Cons(h, t) => t.foldLeft(mapper(h, acc))(mapper)
        case Nil()      => acc


@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
