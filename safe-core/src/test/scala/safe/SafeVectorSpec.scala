package safe

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen

/**
 * Specification tests for [[safe.SafeVector]] 
 */
class SafeVectorSpec extends FlatSpec 
                             with SafeVectorMatchers 
                             with ShouldMatchers 
                             with GeneratorDrivenPropertyChecks {
  
  "A SafeVector" should "equal another SafeVector with the same elements" in {
    val a = SafeVector(1,2,3)
    val b = SafeVector(1,2,3)
    val c = SafeVector(1,1,2,2,3,3)
    
    a should equal (b)
    a should not equal (c)
    a should equal(c(1 to c.length by 2))
  }
  
  it should "be fillable with elements" in {
    val a = SafeVector.fill(2) { 1.0 }
    val b = SafeVector(1.0, 1.0)
    
    a should equal(b)
  }
  
  it should "allow mapping of ranges" in {
    val a = SafeVector.rangeMap(0, 2) { _ + 1 }
    val b = SafeVector.rangeMap(1, 3) { _ + 1 }
    
    a should equal (SafeVector(1,2))
    b should equal (SafeVector(2,3))
  }
  
  it should "allow zero-padding" in {
    val a = SafeVector.zeroPad(SafeVector(1.0, 2.0), 4)
    
    a should equal (SafeVector(1.0, 2.0, 0.0, 0.0))
  }
  
  it should "provide concatenation with other SafeVectors" in {
    val ab = SafeVector(1,2) ++ SafeVector(3,4)
    
    ab should equal (SafeVector(1,2,3,4))
  }
  
  it should "provide mapping over elements" in {
    val a = SafeVector(1,2) map { _ + 1 }
    a should equal (SafeVector(2,3))
  }
  
  it should "be immutable, toArray should copy" in {
    val a = SafeVector(1,2)
    val as = a.toArray
    as(1) = 0
    
    a should equal (SafeVector(1,2))
    as should equal (Array(1,0))
  }
  
  it should "allow slicing" in {
    val a = SafeVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val b = a(0 until a.length by 2)
    val c = a(1 until a.length - 1)
    
    b should equal(SafeVector(1.0, 3.0, 5.0))
    c should equal(SafeVector(2.0, 3.0, 4.0, 5.0))
  }
  
  it should "allow hadamard (element-wise) multiplication" in {
    val ab = SafeVector(1.0, 2.0) :* SafeVector(2.0, 3.0)
    
    ab should equal (SafeVector(2.0, 6.0))
  }
  
  it should "allow addition with other vectors" in {
    val a_plus_b = SafeVector(1,2) + SafeVector(2,3)
    
    a_plus_b should equal (SafeVector(3,5))
  }
  
  it should "allow subtraction with other vectors" in {
    val a_minus_b = SafeVector(2,4) - SafeVector(2,3)
    
    a_minus_b should equal (SafeVector(0,1))
  }
  
  val lengths = for { n <- Gen.choose(0, 100) } yield n
  val factors = for { fac <- Gen.choose(-100.0, 100.0) } yield fac
  
  // Addition and Hadamard mult should be commutative
  it should "have commutative addition and hadamard products" in {
    forAll (lengths, factors) { (n: Int, fac: Double) =>
      whenever (n >= 0) {
        val a = SafeVector.fill(n) { scala.util.Random.nextDouble * fac }
        val b = SafeVector.fill(n) { scala.util.Random.nextDouble * fac }
    
        val a_plus_b = a + b
        val b_plus_a = b + a
        val ab = a :* b
        val ba = b :* a
    
        a_plus_b should equalWithTolerance (b_plus_a, 1e-6)
        ab should equalWithTolerance (ab, 1e-6)
      }
    }
  }
  
  // Addition and Hadamard mult should be associative
  it should "have associative addition and hadamard products" in {
    forAll (lengths, factors) { (n: Int, fac: Double) =>
      whenever (n >= 0) {
        val a = SafeVector.fill(n) { scala.util.Random.nextDouble * fac }
        val b = SafeVector.fill(n) { scala.util.Random.nextDouble * fac }
        val c = SafeVector.fill(n) { scala.util.Random.nextDouble * fac }
    
        val aplusb_plusc = (a + b) + c
        val aplus_bplusc = a + (b + c)
        val ab_c = (a :* b) :* c
        val a_bc = a :* (b :* c)
    
        aplusb_plusc should equalWithTolerance (aplus_bplusc, 1e-6)
        ab_c should equalWithTolerance (a_bc, 1e-6)
      }
    }
  }
}