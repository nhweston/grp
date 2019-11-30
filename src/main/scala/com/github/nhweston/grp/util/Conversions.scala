package com.github.nhweston.grp.util

import com.github.nhweston.grp.data.{Mod, Perm}

object Conversions {

    object Int2Mod {
        implicit def int2Mod[M <: Int] (n: Int) (implicit m: ValueOf[M]) : Mod[M] = Mod (n)
    }

    object IntSeq2PermAsCycle {
        implicit def intSeq2Perm[M <: Int] (cycle: Seq[Int]) (implicit m: ValueOf[M]) : Perm[M] =
            Perm.fromCycle (cycle :_*)
    }

    object IntSeq2PermAsMapping {
        implicit def intSeq2Perm[M <: Int] (mapping: Seq[Int]) (implicit m: ValueOf[M]) : Perm[M] =
            Perm.fromMapping (mapping :_*)
    }

    object IntFunction2Perm {
        implicit def intFunction2Perm[M <: Int] (f: Int => Int) (implicit m: ValueOf[M]) : Perm[M] =
            Perm.fromFunction (f)
    }

}
