package com.github.nhweston.grp.inst

import com.github.nhweston.grp.Grp
import com.github.nhweston.grp.data.Perm

object PermGrp {

    implicit def mkPermGrp[M <: Int] (implicit m: ValueOf[M]) : Grp[Perm[M]] = new Grp[Perm[M]] {
        override def plus (x: Perm[M], y: Perm[M]) : Perm[M] = Perm.fromFunction (x compose y)
        override def negate (x: Perm[M]) : Perm[M] = {
            val fMap = Map.newBuilder[Int, Int]
            for (n <- 0 until m.value) fMap += (x (n) -> n)
            Perm.fromFunction (fMap.result ())
        }
        override def zero: Perm[M] = Perm.fromFunction (identity)
    }

}
