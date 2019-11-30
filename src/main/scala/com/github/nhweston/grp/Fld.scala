package com.github.nhweston.grp

import com.github.nhweston.grp.util.Util._

trait Fld[T] extends Grp[T] {

    def plus (x: T, y: T) : T
    def times (x: T, y: T) : T
    def negate (x: T) : T
    def invert (x: T) : T
    def zero: T
    def one: T

    def pow (x: T, n: Int) : T = {
        if (n < 0) join (invert (x), n) (times)
        else if (n == 0) one
        else join (x, n) (times)
    }

}

object Fld {

    import language.implicitConversions

    class Ops[T] (self: T) (implicit field: Fld[T]) extends Grp.Ops[T] (self) {
        def unary_~ : T = field.invert (self)
        def * (other: T) : T = field.times (self, other)
        def / (other: T) : T = field.times (self, field.invert (other))
    }

    implicit def mkOps[T: Fld] (self: T) : Ops[T] = new Ops (self)

}
