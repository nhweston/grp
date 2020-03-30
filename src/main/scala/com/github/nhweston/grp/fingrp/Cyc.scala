package com.github.nhweston.grp.fingrp

import com.github.nhweston.grp.FinGrp
import com.github.nhweston.grp.Grp._
import com.github.nhweston.grp.data.Mod
import com.github.nhweston.grp.inst.ModFld._

object Cyc {

    def apply[M <: Int] (implicit m: ValueOf[M]) : FinGrp[Mod[M]] = {
        new FinGrp[Mod[M]] {
            override def set: Set[Mod[M]] = (0 until m.value) .map (Mod[M]) .toSet
            override def op: (Mod[M], Mod[M]) => Mod[M] = _ + _
        }
    }

}
