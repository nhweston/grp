package com.github.nhweston.grp.model

import com.github.nhweston.grp.Grp

sealed trait HList

object HList {

    type HNil = HNil.type

    object HNil extends HList {
        def ::[H] (head: H) : HCons[H, HNil] = HCons (head, this)
    }

    case class HCons[H, T <: HList] (head: H, tail: T) extends HList {
        def ::[H0] (head: H0) : HCons[H0, HCons[H, T]] = HCons (head, this)
    }

    type ::[H, T <: HList] = HCons[H, T]

    implicit def mkHNilGrp: Grp[HNil] = {
        new Grp[HNil] {
            override def append (x: HNil, y: HNil) : HNil = HNil
            override def invert(x: HNil): HNil = HNil
            override def zero: HNil = HNil
        }
    }

    implicit def mkHConsGrp[H, T <: HList] (implicit grpH: Grp[H], grpT: Grp[T]) : Grp[H :: T] = {
        import Grp._
        type Self = H :: T
        new Grp[Self] {
            override def append (x: Self, y: Self) : Self = HCons ((x.head + y.head), (x.tail + y.tail))
            override def invert (x: Self) : Self = HCons (-x.head, -x.tail)
            override def zero: Self = HCons (grpH.zero, grpT.zero)
        }
    }

}
