// Task 1 and 2

object Modules extends App :

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def coursesOf(t: Teacher)(persons: Sequence[Person]) =
      flatMap(persons):
        case p@Teacher(_, course) if p.equals(t) => Sequence.Cons(course, Sequence.Nil())
        case _ => Sequence.Nil()

  println(Person.name(Person.Student("mario", 2015)))

  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  println(isStudent(Student("mario", 2015)))

  val viroli: Teacher = Teacher("Viroli", "PPS")
  val persons = Sequence.Cons(viroli, Sequence.Cons(Teacher("Ricci", "PCD"), Sequence.Cons(Student("Tampieri", 1), Sequence.Nil())))
  println(coursesOf(viroli)(persons))

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

// Task 3

object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def fill[A](n: Int)(k: A): Stream[A] =
      if n == 0 then
        Empty()
      else
        cons(k, fill(n - 1)(k))

    /**
     * Calculate Pell numbers using the Binet formula
     * @return a stream over Pell numbers
     */
    def pellCloseFormula(): Stream[Long] =
      Stream.map(Stream.map(Stream.iterate(0)(_ + 1))
        (x => (Math.pow(1 + Math.sqrt(2), x) - Math.pow(1 - Math.sqrt(2), x))/(2*Math.sqrt(2)))
      )(x => Math.round(x))

    def pell(): Stream[Long] =
      def _pell(a: Long)(b: Long): Stream[Long] =
        val current = 2 * b + a
        Cons(() => current, () => _pell(b)(current))
      Cons(() => 0, () => Cons(() => 1, () => _pell(0)(1)))

  end Stream

@main def tryStreams =
  import Streams.* 

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]
