package com.github.nhweston.grp.ops

import com.github.nhweston.grp.Grp
import shapeless.{::, HList, HNil, Nat}
import shapeless.HList._
import shapeless.ops._
import shapeless.ops.hlist._
import shapeless.ops.hlist.Split._
import shapeless.ops.hlist.Prepend._
import shapeless.syntax._
import shapeless.tupled._

object Prod {

    // See: https://stackoverflow.com/questions/32366527/splitting-an-hlist-that-was-concatenated-using-prependa-b
    def unprepend[AB <: HList, N <: Nat, A <: HList, B <: HList] (ab: AB)
    (implicit split: Split.Aux[AB, N, A, B]) : (A, B) = split (ab)

    def mkProdGrp[G <: HList, H <: HList, GH <: HList, N <: Nat] (implicit
        prepend: Prepend.Aux[G, H, GH],
        split: Split.Aux[GH, N, G, H],
        G: Grp[G],
        H: Grp[H]
    ) : Grp[GH] = {
        new Grp[GH] {

            override def plus (x: GH, y: GH) : GH = (unprepend (x), unprepend (y)) match {
                case ((xg, xh), (yg, yh)) => G.plus (xg, yg) ++ H.plus (xh, yh)
            }

            override def negate (x: GH) : GH = unprepend (x) match {
                case (xg, xh) => G.negate (xg) ++ H.negate (xh)
            }

            override def zero: GH = G.zero ++ H.zero
        }
    }

}
