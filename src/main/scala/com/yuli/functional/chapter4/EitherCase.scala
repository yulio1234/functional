package com.yuli.functional.chapter4

class EitherCase {
 def mean(xs:IndexedSeq[Double]):Either[String,Double] =
   if(xs.isEmpty) Left("mean of empty list!")
   else Right(xs.sum/xs.length)

}
