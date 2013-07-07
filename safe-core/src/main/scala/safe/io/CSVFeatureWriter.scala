package safe.io

import safe.SafeVector

object CSVFeatureWriter {
  
  def delimWriteable[A](delim: String, end: String = System.lineSeparator())
                    (implicit w: Writeable[A, String]): Writeable[SafeVector[A], String] = 
    new Writeable[SafeVector[A], String] {
      def apply(as: SafeVector[A]) = as.toArray.map(a => w(a)).mkString("", delim, end)
    }
}