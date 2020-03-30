package com.github.nhweston.grp.fingrp

import com.github.nhweston.grp.FinGrp
import com.github.nhweston.grp.Grp._
import com.github.nhweston.grp.data.Mod
import com.github.nhweston.grp.inst.ModFld._

object Dih {

    def apply[M <: Int] (implicit m: ValueOf[M]) : FinGrp[(Mod[M], Mod[2])] = {
        type Elem = (Mod[M], Mod[2])
        def Elem (x1: Int, x2: Int) : Elem = (Mod (x1), Mod (x2))
        new FinGrp[Elem] {
            override def set: Set[Elem] =
                (0 until m.value) .flatMap (m0 => Set (Elem (m0, 0), Elem (m0, 1))) .toSet
            override def op: (Elem, Elem) => Elem = {
                case ((x1, x2), (y1, y2)) => (
                    if (x2.value == 0) x1 + y1 else x1 - y1,
                    x2 + y2
                )
            }
        }
    }

}
