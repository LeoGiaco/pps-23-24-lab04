package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    private case class ComplexImpl(re: Double, im: Double) 
    opaque type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexImpl(re, _) => re
      def im(): Double = complex match
        case ComplexImpl(_, im) => im
      def sum(other: Complex): Complex = ComplexImpl(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex = ComplexImpl(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case ComplexImpl(re, 0) => "%.1f".formatted(re)
        case ComplexImpl(0, im) => "%.1fi".formatted(im)
        case ComplexImpl(re, im) => 
            val sign = if complex.im() < 0 then "-" else "+"
            "%.1f %s %.1fi".formatted(re, sign, Math.abs(im))

            