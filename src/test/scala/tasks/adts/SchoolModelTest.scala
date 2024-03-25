package tasks.adts

import org.junit.*
import org.junit.Assert.*

import SchoolModel.*
import u03.Optionals.Optional
import u03.EncapsulatedSequences.Sequence.map
import u03.BTrees.Tree.count

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
        
}
