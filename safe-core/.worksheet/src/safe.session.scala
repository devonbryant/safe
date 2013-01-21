package safe

object session {
	import scalaz._
	import Scalaz._
	import ImmutableArray._
  import safe.math._;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(225); 
  
  val d = Array(Complex(1), Complex(1), Complex(1), Complex(1), Complex(0), Complex(0), Complex(0), Complex(0));System.out.println("""d  : Array[safe.math.Complex] = """ + $show(d ));$skip(16); val res$0 = 
  
  FFT.fft(d);System.out.println("""res0: Seq[safe.math.Complex] = """ + $show(res$0))}
  
}
