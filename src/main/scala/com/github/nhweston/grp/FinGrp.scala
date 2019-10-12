package com.github.nhweston.grp

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

trait FinGrp[T] extends Grp[T] {

    def set: Set[T]
    def op: (T, T) => T
    implicit def ord: Ordering[T]

    override def append (x: T, y: T) : T = table ((x, y))
    override def invert (x: T) : T = inverses (x)

    lazy val seq: Seq[T] = (mutable.PriorityQueue from set) .toSeq

    lazy val table: Map[(T, T), T] = {
        val builder = Map.newBuilder[(T, T), T]
        for ((a, b) <- seq zip seq) builder += ((a, b) -> op (a, b))
        builder.result ()
    }

    override lazy val zero: T = {
        seq.filter (e => seq.forall (a => table ((e, a)) == a)) match {
            case Seq (e) => e
            case _ => throw new IllegalArgumentException ("Definition is non-unital")
        }
    }

    lazy val inverses: Map[T, T] = {
        val builder = Map.newBuilder[T, T]
        for (a <- seq) {
            seq.filter (b => table ((a, b)) == zero) match {
                case Seq (b) => builder += (a -> b)
                case _ => throw new IllegalArgumentException ("Definition is non-invertible")
            }
        }
        builder.result ()
    }

}

object FinGrp {

    def generate[T] (
        iden: T,
        gtors: Set[T],
        op: (T, T) => T
    ) (implicit ord: Ordering[T]) : FinGrp[T] = {
        val op0 = op
        val ord0 = ord
        new FinGrp[T] {
            override def set: Set[T] = {
                @tailrec
                def aux (set: Set[T], q: Queue[T]) : Set[T] = {
                    q match {
                        case head +: tail =>
                            if (set contains head) aux (set, tail)
                            else aux (set + head, tail :++ gtors.map (op (head, _)))
                        case Queue () => set
                    }
                }
                aux (Set.empty, Queue (iden))
            }
            override def op: (T, T) => T = op0
            override implicit def ord: Ordering[T] = ord0
        }
    }

}
