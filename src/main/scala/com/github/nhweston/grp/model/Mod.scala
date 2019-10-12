package com.github.nhweston.grp.model

import com.github.nhweston.grp.{Field, Grp}

import scala.annotation.tailrec

class Mod[M <: Int] private (val n: Int) extends AnyVal {

    override def toString: String = n.toString

}

object Mod {

    def apply[M <: Int] (n: Int) (implicit m: ValueOf[M]) = new Mod[M] (math.floorMod (n, m.value))

    implicit def mkModGrp[M <: Int] (implicit m: ValueOf[M]) : Grp[Mod[M]] = new Grp[Mod[M]] {
        override def append (x: Mod[M], y: Mod[M]) : Mod[M] = Mod (x.n + y.n)
        override def invert (x: Mod[M]) : Mod[M] = Mod (-x.n)
        override def zero: Mod[M] = Mod (0)
    }

    implicit def mkModField[M <: Int] (implicit m: ValueOf[M]) : Field[Mod[M]] = new Field[Mod[M]] {
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
