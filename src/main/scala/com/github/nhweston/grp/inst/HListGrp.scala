package com.github.nhweston.grp.inst

import com.github.nhweston.grp.Grp
import shapeless.{HList, HNil, ::}

object HListGrp {

    implicit def mkHNilGrp: Grp[HNil] = new Grp[HNil] {
        override def plus (x: HNil, y: HNil) : HNil = HNil
        override def negate (x: HNil) : HNil = HNil
        override def zero: HNil = HNil
    }

    implicit def mkHConsGrp[H, T <: HList] (implicit grpH: Grp[H], grpT: Grp[T]) : Grp[H :: T] = {
        type Self = H :: T
        new Grp[Self] {
            override def plus (x: Self, y: Self) : Self = {
                val (hdX :: tlX, hdY :: tlY) = (x, y)
                grpH.plus (hdX, hdY) :: grpT.plus (tlX, tlY)
            }
            override def negate (x: Self) : Self = {
                val (hd :: tl) = x
                grpH.negate (hd) :: grpT.negate (tl)
            }
            override def zero: Self = grpH.zero :: grpT.zero
        }
    }

}
