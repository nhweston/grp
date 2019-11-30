package com.github.nhweston.grp.data

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

case class Perm[M <: Int] private (private val f: ArraySeq[Int]) (implicit m: ValueOf[M]) extends (Int => Int) {

    override def apply (n: Int) : Int = f (math.floorMod (n, m.value))

    lazy val cycles: Seq[Seq[Int]] = {
        @tailrec
        def buildCycle (
            head: Int,
            n: Int,
            cycle: Seq[Int]
        ) : Seq[Int] =
            if (head == n) head +: cycle
            else buildCycle (head, f (n), n +: cycle)
        @tailrec
        def aux (
            n: Int,
            result: Seq[Seq[Int]],
            included: Set[Int]
        ) : Seq[Seq[Int]] =
            if (included contains n) aux (n + 1, result, included)
            else if (n >= m.value) result
            else {
                val cycle = buildCycle (n, f (n), Seq.empty)
                if (cycle.size < 2) aux (n + 1, result, included)
                else aux (n + 1, result :+ cycle, included ++ cycle)
            }
        aux (0, Seq.empty, Set.empty)
    }

    override lazy val toString: String =
        cycles match {
            case Nil => "()"
            case cycles => cycles.map (cycle => cycle.mkString ("(", " ", ")")) .mkString (" ")
        }

    override def equals (o: Any) : Boolean =
        o match {
            case other: Perm[M] => (this.f zip other.f) .forall {case (a, b) => a == b}
            case _ => false
        }

    override def hashCode: Int = f.hashCode

}

object Perm {

    def zero[M <: Int] (implicit m: ValueOf[M]) : Perm[M] = new Perm[M] (ArraySeq from (0 until m.value))
    def sigma[M <: Int] (implicit m: ValueOf[M]) : Perm[M] = new Perm (ArraySeq from ((1 until m.value) :+ 0))
    def tau[M <: Int] (implicit m: ValueOf[M]) : Perm[M] = new Perm (ArraySeq from (1 +: 0 +: (2 until m.value)))

    def fromCycle[M <: Int] (cycle: Int*) (implicit m: ValueOf[M]) : Perm[M] =
        (cycle.map (math.floorMod (_, m.value))) match {
            case cycle @ (head +: tail) =>
                val fMap = ((tail :+ head) zip cycle) .toMap
                new Perm (ArraySeq from ((0 until m.value) .map (n => fMap.getOrElse (n, n))))
            case Nil => new Perm (ArraySeq from (0 until m.value))
        }

    def fromFunction[M <: Int] (f: Int => Int) (implicit m: ValueOf[M]) : Perm[M] =
        new Perm (ArraySeq from ((0 until m.value) .map (n => math.floorMod (f (n), m.value))))

    def fromMapping[M <: Int] (indices: Int*) (implicit m: ValueOf[M]) : Perm[M] =
        fromFunction (n => indices.applyOrElse[Int, Int] (n, _ => n))

}
