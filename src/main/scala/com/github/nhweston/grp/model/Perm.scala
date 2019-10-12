package com.github.nhweston.grp.model

import com.github.nhweston.grp.Grp

import scala.annotation.tailrec

class Perm[M <: Int] private (private val f: Vector[Int]) (implicit m: ValueOf[M]) extends (Int => Int) {

    override def apply (n: Int) : Int = f (math.floorMod (n, m.value))

    def cycles: Seq[Seq[Int]] = {
        @tailrec
        def buildCycle (n: Int, cycle: Seq[Int] = Seq.empty) : Seq[Int] = {
            if (cycle.headOption contains n) cycle
            else buildCycle (f (n), cycle :+ n)
        }
        @tailrec
        def aux (n: Int, result: Seq[Seq[Int]], included: Set[Int]) : Seq[Seq[Int]] = {
            if (included contains n) aux (n + 1, result, included)
            else if (n >= m.value) result
            else {
                val cycle = buildCycle (n)
                if (cycle.size < 2) aux (n + 1, result, included)
                else aux (n + 1, result :+ cycle, included ++ cycle)
            }
        }
        aux (0, Seq.empty, Set.empty)
    }

    override def toString: String = cycles.map (cycle => cycle.mkString ("(", " ", ")")) .mkString (" ")

    override def equals (o: Any) : Boolean = {
        o match {
            case other: Perm[M] => (this.f zip other.f) .forall {case (a, b) => a == b}
            case _ => false
        }
    }

    override def hashCode: Int = f.hashCode

}

object Perm {

    def cyc[M <: Int] (cycle: Int*) (implicit m: ValueOf[M]) : Perm[M] = {
        (cycle.map (math.floorMod (_, m.value))) match {
            case cycle @ (head +: tail) =>
                val fMap = (cycle zip (tail :+ head)) .toMap
                new Perm ((0 until m.value) .map (n => fMap.getOrElse (n, 0)) .toVector)
            case Nil => new Perm ((0 until m.value) .toVector)
        }
    }

    def apply[M <: Int] (f: Int => Int) (implicit m: ValueOf[M]) : Perm[M] = {
        new Perm ((0 until m.value) .map (n => math.floorMod (f (n), m.value)) .toVector)
    }

    implicit def mkGrp[M <: Int] (implicit m: ValueOf[M]) : Grp[Perm[M]] = new Grp[Perm[M]] {
        override def append (x: Perm[M], y: Perm[M]) : Perm[M] = Perm (x compose y)
        override def invert (x: Perm[M]) : Perm[M] = {
            val fMap = Map.newBuilder[Int, Int]
            for (n <- 0 until m.value) fMap += (x (n) -> n)
            Perm (fMap.result ())
        }
        override def zero: Perm[M] = Perm (identity)
    }

}
