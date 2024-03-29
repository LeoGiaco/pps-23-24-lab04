package u04lab
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.Optional
import Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()
    
  trait Traversable[T[_]]:
    extension [A](el: T[A])
      def consume(func: A => Unit): Unit

  given Traversable[Optional] with
    extension [A](el: Optional[A])
      def consume(func: A => Unit): Unit = el match
        case Optional.Just(v) => func(v)
        case Optional.Empty() => ()
  
  given Traversable[Sequence] with
    extension [A](el: Sequence[A])
      def consume(func: A => Unit): Unit = el match
        case Cons(h, t) => func(h); t.consume(func)
        case Nil() => (9)
    
  @main def tryTraversables =
    val seq1 = Cons("a", Cons("b", Cons("c", Cons("d", Nil()))))
    seq1.consume(log)
    val seq2 = Cons(10, Cons(20, Cons(30, Cons(40, Nil()))))
    seq2.consume(log)
    val opt1 = Just("prova")
    opt1.consume(log)
    val opt2 = Just(22)
    opt2.consume(log)
    val opt3 = Empty()
    opt3.consume(log)



  
