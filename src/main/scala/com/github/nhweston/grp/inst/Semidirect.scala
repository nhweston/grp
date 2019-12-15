package com.github.nhweston.grp.inst

import com.github.nhweston.grp.Grp
import com.github.nhweston.grp.Grp._
import com.github.nhweston.grp.data.Action

object Semidirect {

    def mkSemidirectGrp[T, U] (f: U => Action[T])
    (implicit grpT: Grp[T], grpU: Grp[U]) : Grp[(T, U)] = {
        type Self = (T, U)
        new Grp[Self] {

            override def plus (x: Self, y: Self) : Self = {
                val ((xT, xU), (yT, yU)) = (x, y)
                (xT + f (xU) (yT), xU + yU)
            }

            override def negate (x: Self) : Self = {
                val (xT, xU) = x
                (f (xU) .inv (-xT), -xU)
            }

            override def zero: Self = (grpT.zero, grpU.zero)

        }
    }

}
