package com.github.nhweston.grp

trait Field[T] {

    def plus (x: T, y: T) : T
    def times (x: T, y: T) : T
    def negate (x: T) : T
    def invert (x: T) : T
    def zero: T
    def one: T

}

object Field {

    import language.implicitConversions

    class Ops[T] (self: T) (implicit field: Field[T]) {
        def unary_- : T = field.negate (self)
        def unary_~ : T = field.invert (self)
        def + (other: T) : T = field.plus (self, other)
        def - (other: T) : T = field.plus (self, field.negate (other))
        def * (other: T) : T = field.times (self, other)
        def / (other: T) : T = field.times (self, field.invert (other))
    }

    implicit def mkOps[T: Field] (self: T) : Ops[T] = new Ops (self)

    def mkAddGrp[T] (implicit field: Field[T]) : Grp[T] = new Grp[T] {
        override def append (x: T, y: T) : T = x + y
        override def invert (x: T) : T = -x
        override def zero: T = field.zero
    }

    def mkMulGrp[T] (implicit field: Field[T]) : Grp[T] = new Grp[T] {
        override def append (x: T, y: T) : T = x * y
        override def invert (x: T): T = ~x
        override def zero: T = field.one
    }

}
