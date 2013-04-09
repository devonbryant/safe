package safe

package object dsp {
  
  val lnOf2 = math.log(2)
  def log2(x: Double) = math.log(x) / lnOf2
  
  def ispow2(n: Int) = ((n & n - 1) == 0)
  def nextpow2(n: Int) = {
    var i = 1
    while (math.pow(2, i) < n) i += 1
    i
  }
  
  def expc(c: breeze.math.Complex) = {
    val er = math.exp(c.real)
    breeze.math.Complex(er * math.cos(c.imag), er * math.sin(c.imag))
  }
                        
}