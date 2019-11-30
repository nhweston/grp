package com.github.nhweston.grp.data

final class Mod[M <: Int] private (val n: Int) extends AnyVal {

    def value: Int = n
    def modulus (implicit m: ValueOf[M]) : Int = m.value

    override def toString: String = n.toString

}

object Mod {

    def apply[M <: Int] (n: Int) (implicit m: ValueOf[M]) = new Mod[M] (math.floorMod (n, m.value))

}
