package com.github.nhweston.grp

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, SortedMap}
import scala.collection.mutable

trait FinGrp[T] extends Grp[T] {

    def set: Set[T]
    def op: (T, T) => T

    override def plus (x: T, y: T) : T = table ((x, y))
    override def negate (x: T) : T = inverses (x)
    override def abs (x: T) : Int = orders (x)

    private lazy val seq: Seq[T] = set.toSeq

    lazy val table: Map[(T, T), T] = {
        val builder = Map.newBuilder[(T, T), T]
        for (a <- seq; b <- seq) builder += ((a, b) -> op (a, b))
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

    lazy val orders: Map[T, Int] = {
        val builder = Map.newBuilder[T, Int]
        for (a <- seq) builder += (a -> super.abs (a))
        builder.result ()
    }

    lazy val orderProfile: SortedMap[Int, Int] = {
        val mmap = mutable.Map.empty[Int, Int]
        for ((_, o) <- orders) {
            mmap.updateWith (o) {
                case Some (n) => Some (n + 1)
                case None => Some (1)
            }
        }
        SortedMap from mmap
    }

    lazy val conjugacyClasses: Seq[Set[T]] = {
        def from (a: T) : Set[T] = for (b <- set) yield plus (plus (negate (b), a), b)
        @tailrec
        def aux (
            result: Seq[Set[T]],
            remaining: Set[T]
        ) : Seq[Set[T]] = {
            remaining.headOption match {
                case Some (a) =>
                    val next = from (a)
                    aux (result :+ next, remaining -- next)
                case None => result
            }
        }
        aux (Seq (Set (zero)), set - zero)
    }

    def findGtors: Set[T] = {
        @tailrec
        def gen (
            gtor: T,
            set: Set[T],
            q: Queue[T]
        ) : Set[T] = {
            q match {
                case head +: tail =>
                    if (set contains head) gen (gtor, set, tail)
                    else gen (gtor, set + head, q :+ op (head, gtor))
                case Queue () => set
            }
        }
        @tailrec
        def aux (
            gted: Set[T],
            ungted: Set[T],
            result: Set[T]
        ) : Set[T] = {
            if (ungted.isEmpty) result
            else {
                val gtor = ungted.maxBy (abs)
                val next = gen (gtor, Set.empty, Queue from gted)
                aux (next, ungted -- next, result + gtor)
            }
        }
        aux (Set (zero), set - zero, Set.empty)
    }

}

object FinGrp {

    def gen[T] (zero: T, gtors: Set[T], op: (T, T) => T) : Set[T] = {
        @tailrec
        def aux (set: Set[T], q: Queue[T]) : Set[T] = {
            q match {
                case head +: tail =>
                    val setNext = set + head
                    val qNext = tail ++ gtors.map (op (head, _)) .filterNot (setNext)
                    aux (setNext, qNext)
                case Queue () => set
            }
        }
        aux (Set.empty, Queue (zero))
    }

    def genToGrp[T] (zero: T, gtors: Set[T], op: (T, T) => T) : FinGrp[T] = {
        val _zero = zero
        val _op = op
        new FinGrp[T] {
            override lazy val set: Set[T] = gen (_zero, gtors, _op)
            override lazy val op: (T, T) => T = _op
        }
    }

}
