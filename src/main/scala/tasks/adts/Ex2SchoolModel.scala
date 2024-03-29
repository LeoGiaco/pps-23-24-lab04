package tasks.adts
import u03.Sequences.*
import u03.Optionals.*
import Sequence.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:
  trait SchoolModule:
    type School
    type Teacher
    type Course
    def createSchool(): School
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

import tasks.adts.SchoolModel.SchoolModule

object BaseSchoolModel extends SchoolModule:
  private case class SchoolImpl(teachers: Sequence[TeacherImpl], courses: Sequence[CourseImpl])
  private case class TeacherImpl(name: String, courses: Sequence[CourseImpl])
  private case class CourseImpl(name: String)

  opaque type School = SchoolImpl
  opaque type Teacher = TeacherImpl
  opaque type Course = CourseImpl

  def createSchool(): School = SchoolImpl(Nil(), Nil())
  extension (school: School)
    def addTeacher(name: String): School = 
      val teacher = TeacherImpl(name, Nil())
      school match
        case SchoolImpl(Nil(), c) => SchoolImpl(Cons(teacher, Nil()), c)
        case SchoolImpl(t, c) => SchoolImpl(Cons(teacher, t), c)
    
    def addCourse(name: String): School =
      val course = CourseImpl(name)
      school match
        case SchoolImpl(t, Nil()) => SchoolImpl(t, Cons(course, Nil()))
        case SchoolImpl(t, c) => SchoolImpl(t, Cons(course, c))

    def teacherByName(name: String): Optional[Teacher] =
      def findTeacher(name: String, teachers: Sequence[Teacher]): Optional[Teacher] = teachers match
        case Cons(TeacherImpl(n, c), _) if n == name  => Optional.Just(TeacherImpl(n, c))
        case Cons(_, t)                               => findTeacher(name, t)
        case Nil()                                    => Optional.Empty()
      school match
        case SchoolImpl(teachers, _) => findTeacher(name, teachers)

    def courseByName(name: String): Optional[Course] = 
      def findCourse(name: String, courses: Sequence[Course]): Optional[Course] = courses match
        case Cons(CourseImpl(n), _) if n == name  => Optional.Just(CourseImpl(n))
        case Cons(_, t)                               => findCourse(name, t)
        case Nil()                                    => Optional.Empty()
      school match
        case SchoolImpl(_, courses) => findCourse(name, courses)

    def nameOfTeacher(teacher: Teacher): String = teacher match
      case TeacherImpl(name, _) => name
    
    def nameOfCourse(course: Course): String = course match
      case CourseImpl(name) => name
    
    def setTeacherToCourse(teacher: Teacher, course: Course): School = 
      val mapTeachers: Teacher => Teacher = (t) => (t, teacher) match
          case (TeacherImpl(n1, c), TeacherImpl(n2, _)) if n1 == n2 => TeacherImpl(n1, Cons(course, c))
          case _ => t
      school.match
        case SchoolImpl(teachers, courses) if teachers != Sequence.Nil() && courses != Sequence.Nil() =>
          SchoolImpl(Sequence.map(teachers: Sequence[TeacherImpl])(mapTeachers), courses)
        case _ => school

    def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
      def findTeacher(teacher: Teacher, teachers: Sequence[Teacher]): Sequence[Course] = (teacher, teachers) match
        case (TeacherImpl(n1, _), Cons(TeacherImpl(n2, c), t)) if n1 == n2 => c
        case (tc, Cons(_, t)) => findTeacher(tc, t)
        case (_, Nil()) => Nil()
      school match
        case SchoolImpl(s, _) => findTeacher(teacher, s)
      
    