package tasks.adts

import org.junit.*
import org.junit.Assert.*

import SchoolModel.*
import u03.Optionals.Optional
import u03.Sequences.*

class SchoolModelTest {
    
    val schoolModel = BaseSchoolModel
    import schoolModel.* 

    @Test def testAddTeacher() =
        val teacher = "Mario"
        val school = createSchool().addTeacher(teacher)
        val teacherObj: Teacher = school.teacherByName(teacher) match
            case Optional.Just(t) => t
            case _ => ???
        assertEquals(teacher, school.nameOfTeacher(teacherObj))        

    @Test def testAddCourse() =
        val course = "Informatica"
        val school = createSchool().addCourse(course)
        val courseObj: Course = school.courseByName(course) match
            case Optional.Just(c) => c
            case _ => ???
        assertEquals(course, school.nameOfCourse(courseObj))        

    def first[A](sequence: Sequence[A]): Optional[A] = sequence match
        case Sequence.Cons(h, _) => Optional.Just(h)
        case _ => Optional.Empty()


    @Test def assignCourseToTeacherTest() = 
        val teacher = "Mario"
        val course = "Informatica"
        val school = createSchool().addTeacher(teacher).addCourse(course)
        val teacherObj: Teacher = school.teacherByName(teacher) match
            case Optional.Just(t) => t
            case _ => ???
        val courseObj: Course = school.courseByName(course) match
            case Optional.Just(c) => c
            case _ => ???
        val schoolWithTeacherAssigned = school.setTeacherToCourse(teacherObj, course = courseObj)
        val firstCourseObj = first(schoolWithTeacherAssigned.coursesOfATeacher(teacherObj)) match
            case Optional.Just(c) => c
            case _ => ???
        assertEquals(course, schoolWithTeacherAssigned.nameOfCourse(firstCourseObj))    
}
