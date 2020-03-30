package com.github.nhweston.grp.inst

import com.github.nhweston.grp.Fld
import com.github.nhweston.grp.Fld._
import com.github.nhweston.grp.data.Mat

import scala.annotation.tailrec

object MatFld {

    implicit def mkMatFld[D <: Int, T] (implicit d: ValueOf[D], fld: Fld[T]) : Fld[Mat[D, T]] = {
        val dim = d.value
        val size = dim * dim
        def toCoords (idx: Int) : (Int, Int) = (idx / dim, idx % dim)
        type Self = Mat[D, T]
        new Fld[Self] {
            override def plus (x: Self, y: Self) : Self = Mat ((x.elems zip y.elems) .map {case (a, b) => a + b} :_*)
            override def times (x: Self, y: Self) : Self = Mat (
                (0 until size) .map { idx =>
                    val (j, i) = toCoords (idx)
                    (0 until dim) .foldLeft (fld.zero) {
                        case (partial, k) => partial + x (j, k) * y (k, i)
                    }
                } :_*
            )
            override def negate (x: Self) : Self = Mat (x.elems.map (-_) :_*)
            override def invert (x: Self): Self = {
                // FIXME: This is not very smart and only works for matrices of finite order under multiplication.
                @tailrec
                def aux (prev: Self, curr: Self) : Self = {
                    if (curr == zero) prev
                    else aux (curr, times (curr, x))
                }
                aux (x, times (x, x))
            }
            override lazy val zero: Self = Mat (Seq.fill (size) (fld.zero) :_*)
            override lazy val one: Self = Mat (
                (0 until size) .map {
                    toCoords (_) match {
                        case (j, i) if (j == i) => fld.one
                        case _ => fld.zero
                    }
                } :_*
            )
        }
    }

    implicit class MatFldOps[D <: Int, T] (self: Mat[D, T]) (implicit fld: Fld[T]) {

        lazy val det: T = {
            def aux (mat: Seq[Seq[T]]) : T = {
                mat.size match {
                    case 0 => fld.zero
                    case 1 => mat (0) (0)
                    case n =>
                        (0 until n) .foldLeft (fld.zero) { (result, idx) =>
                            val hd +: tl = mat
                            result + hd (idx) * aux (
                                tl.map {
                                    _ splitAt idx match {
                                        case (pre, _ +: post) => pre ++ post
                                    }
                                }
                            )
                        }
                }
            }
            aux ((0 until self.dim) .iterator.map (self.row (_) .toSeq) .toSeq)
        }

    }

}
