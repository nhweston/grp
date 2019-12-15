package com.github.nhweston.grp.inst

import com.github.nhweston.grp.Grp
import shapeless.ops.hlist.{Prepend, Split}
import shapeless.{HList, Nat}

object HListFlatGrp {

    // See: https://stackoverflow.com/questions/32366527/splitting-an-hlist-that-was-concatenated-using-prependa-b
    def unprepend[TU <: HList, N <: Nat, T <: HList, U <: HList] (ab: TU)
    (implicit split: Split.Aux[TU, N, T, U]) : (T, U) = split (ab)

    // Can't make this implicit because of ambiguity.
    def mkHListFlatGrp[T <: HList, U <: HList, TU <: HList, N <: Nat] (implicit
        prepend: Prepend.Aux[T, U, TU],
        split: Split.Aux[TU, N, T, U],
        grpT: Grp[T],
        grpU: Grp[U]
    ) : Grp[TU] = {
        new Grp[TU] {

            override def plus (x: TU, y: TU) : TU = (unprepend (x), unprepend (y)) match {
                case ((xg, xh), (yg, yh)) => grpT.plus (xg, yg) ++ grpU.plus (xh, yh)
            }

            override def negate (x: TU) : TU = unprepend (x) match {
                case (xg, xh) => grpT.negate (xg) ++ grpU.negate (xh)
            }

            override def zero: TU = grpT.zero ++ grpU.zero

        }
    }

}
