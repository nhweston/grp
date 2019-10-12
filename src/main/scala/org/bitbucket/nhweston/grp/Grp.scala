package org.bitbucket.nhweston.grp

import scala.annotation.tailrec

trait Grp[T] {

    def append (x: T, y: T) : T
    def invert (x: T) : T
    def zero: T

    def join (x: T, n: Int) : T = {
        if (n < 0) join (invert (x), -n)
        else if (n == 0) zero
        else {
            val sqrt = join (x, n/2)
            val squared = append (sqrt, sqrt)
            if (n % 2 == 0) squared
            else append (squared, x)
        }
    }

    def order (x: T) : Int = {
        @tailrec
        def aux (current: T, result: Int) : Int = {
            if (current == zero) result
            else aux (append (current, x), result + 1)
        }
        aux (x, 1)
    }

}

object Grp {

    class Ops[T] (self: T) (implicit grp: Grp[T]) {
        def unary_- : T = grp.invert (self)
        def + (other: T) : T = grp.append (self, other)
        def - (other: T) : T = grp.append (self, grp.invert (other))
        def * (n: Int) : T = grp.join (self, n)
    }

    implicit def mkOps[T: Grp] (self: T) : Ops[T] = new Ops (self)

}
