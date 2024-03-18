package u02

import u03.Sequences.Sequence
import u03.Sequences.Sequence.flatMap

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