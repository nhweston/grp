package com.github.nhweston.grp.model

import com.github.nhweston.grp.Grp

object Prod {

    implicit def mkProdGrp[T, U] (implicit grpT: Grp[T], grpU: Grp[U]) : Grp[(T, U)] = {
        import Grp._
        new Grp[(T, U)] {
            override def append (x: (T, U), y: (T, U)) : (T, U) = (x._1 + y._1, x._2 + y._2)
            override def invert (x: (T, U)) : (T, U) = (-x._1, -x._2)
            override def zero: (T, U) = (grpT.zero, grpU.zero)
        }
    }

}
