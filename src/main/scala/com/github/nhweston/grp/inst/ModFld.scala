package com.github.nhweston.grp.inst

import com.github.nhweston.grp.Fld
import com.github.nhweston.grp.data.Mod

import scala.annotation.tailrec

object ModFld {

    implicit def mkModFld[M <: Int] (implicit m: ValueOf[M]) : Fld[Mod[M]] = new Fld[Mod[M]] {
        override def plus (x: Mod[M], y: Mod[M]) : Mod[M] = Mod (x.n + y.n)
        override def times (x: Mod[M], y: Mod[M]) : Mod[M] = Mod (x.n * y.n)
        override def negate (x: Mod[M]) : Mod[M] = Mod (-x.n)
        override def invert (x: Mod[M]) : Mod[M] = {
            @tailrec
            def aux (t0: Int, t1: Int, r0: Int, r1: Int) : Int = {
                if (r1 == 0) {
                    if (r0 > 1) throw new ArithmeticException (s"${x.n} has no inverse modulo ${m.value}")
                    else t0
                }
                else {
                    val q = r0 / r1
                    aux (t1, t0 - q * t1, r1, r0 - q * r1)
                }
            }
            Mod (aux (0, 1, m.value, x.n))
        }
        override def zero: Mod[M] = Mod (0)
        override def one: Mod[M] = Mod (1)
    }

}
