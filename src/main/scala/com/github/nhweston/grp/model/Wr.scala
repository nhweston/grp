package com.github.nhweston.grp.model

import com.github.nhweston.grp.{FinGrp, Grp}

class Wr[T, M <: Int] private (val elems: Vector[T], val perm: Perm[M]) {

    override def toString: String = {
        val strElems = elems.mkString ("(", ", ", ")")
        s"($strElems, $perm)"
    }

    override def equals (o: Any) : Boolean = {
        o match {
            case other: Wr[T, M] =>
                this.perm == other.perm && (this.elems zip other.elems) .forall {case (a, b) => a == b}
            case _ => false
        }
    }

    override def hashCode: Int = (elems, perm) .hashCode

}

object Wr {

    def apply[T, M <: Int] (elems: T*) (perm: Perm[M]) (implicit grp: Grp[T], m: ValueOf[M]) : Wr[T, M] = {
        val dim = m.value
        val vec = {
            if (elems.size < dim) elems.padTo (dim, grp.zero)
            else if (elems.size > dim) elems take dim
            else elems
        } .toVector
        new Wr (vec, perm)
    }

    implicit def mkWrGrp[T, M <: Int] (implicit grp: Grp[T], m: ValueOf[M]) : Grp[Wr[T, M]] = {
        import Grp._
        type Self = Wr[T, M]
        new Grp[Self] {
            override def append (x: Self, y: Self) : Self = {
                new Wr (
                    x.elems.zipWithIndex.map {
                        case (a, i) =>
                            val b = y.elems (x.perm (i))
                            a + b
                    },
                    x.perm + y.perm
                )
            }
            override def invert (x: Self) : Self = ???
            override def zero: Self = new Wr (Vector.fill (m.value) (grp.zero), Perm.zero)
        }
    }

}
