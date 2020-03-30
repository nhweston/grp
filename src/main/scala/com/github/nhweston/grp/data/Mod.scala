package com.github.nhweston.grp.data

final class Mod[M <: Int] private (val n: Int) extends AnyVal {

    def value: Int = n
    def modulus (implicit m: ValueOf[M]) : Int = m.value

    override def toString: String = n.toString

//    override def equals (o: Any) : Boolean = {
//        o match {
//            case other: Mod[M] => this.value == other.value
//            case _ => false
//        }
//    }

}

object Mod {

    def apply[M <: Int] (n: Int) (implicit m: ValueOf[M]) = new Mod[M] (math.floorMod (n, m.value))

}
