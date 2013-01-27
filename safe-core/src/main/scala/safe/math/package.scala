package safe

package object math {
  implicit object ComplexNumeric extends Numeric[Complex] {
    override def plus(x: Complex, y: Complex): Complex = x + y
    override def minus(x: Complex, y: Complex): Complex = x - y
    override def times(x: Complex, y: Complex): Complex = x * y
    override def negate(x: Complex): Complex = -x
    override def fromInt(x: Int): safe.math.Complex = Complex(x)
    override def toDouble(x: safe.math.Complex): Double = ???
    override def toFloat(x: safe.math.Complex): Float = ???
    override def toInt(x: safe.math.Complex): Int = ???
    override def toLong(x: safe.math.Complex): Long = ???
    override def compare(x: Complex, y: Complex) = x.re.compare(y.re) match {
      case 0 => x.im.compare(y.im)
      case n => n
    }
  }
}