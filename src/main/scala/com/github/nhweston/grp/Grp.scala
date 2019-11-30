package com.github.nhweston.grp

import scala.annotation.tailrec

import com.github.nhweston.grp.util.Util._

trait Grp[T] {

    def plus (x: T, y: T) : T
    def negate (x: T) : T
    def zero: T

    def times (x: T, n: Int) : T = {
        if (n < 0) join (negate (x), n) (plus)
        else if (n == 0) zero
        else join (x, n) (plus)
    }

    def abs (x: T) : Int = {
        @tailrec
        def aux (current: T, result: Int) : Int = {
            if (current == zero) result
            else aux (plus (current, x), result + 1)
        }
        aux (x, 1)
    }

    def generate (gtors: Set[T]) : FinGrp[T] = FinGrp.genToGrp (zero, gtors, plus)
    def generate (gtors: T*) : FinGrp[T] = generate (gtors.toSet)

}

object Grp {

    import language.implicitConversions

    class Ops[T] (self: T) (implicit grp: Grp[T]) {
        def unary_- : T = grp.negate (self)
        def + (other: T) : T = grp.plus (self, other)
        def - (other: T) : T = grp.plus (self, grp.negate (other))
        def * (n: Int) : T = grp.times (self, n)
    }

    implicit def mkOps[T: Grp] (self: T) : Ops[T] = new Ops (self)

}
