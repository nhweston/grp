package com.github.nhweston.grp.model

import com.github.nhweston.grp.Field
import com.github.nhweston.grp.Field._

class Mat[Y <: Int, X <: Int, T] private (val elems: Seq[T]) (implicit y: ValueOf[Y], x: ValueOf[X]) {

    def dimY: Y = y.value
    def dimX: X = x.value
    def dim: (Int, Int) = (y.value, x.value)

    def toIndex (j: Int, i: Int) : Int = {
        if (j < 0 || j >= dimY || i < 0 || i >= dimX) throw new IndexOutOfBoundsException (s"($j, $i)")
        else i + j * dimX
    }

    def toCoords (index: Int) : (Int, Int) = {
        if (index < 0 || index > elems.length) throw new IndexOutOfBoundsException (s"$index")
        else (index / dimX, index % dimX)
    }

    def apply (j: Int, i: Int) : T = elems (toIndex (j, i))

    def column (i: Int) : Iterator[T] = new Iterator[T] {
        private var count = 0
        private var index = toIndex (0, i)
        override def hasNext: Boolean = count < dimX
        override def next () : T = {
            val result = elems (index)
            count += 1
            index += dimX
            result
        }
    }

    def row (j: Int) : Iterator[T] = new Iterator[T] {
        private var count = 0
        private var index = toIndex (j, 0)
        override def hasNext: Boolean = count < dimY
        override def next () : T = {
            val result = elems (index)
            count += 1
            index += 1
            result
        }
    }

}

object Mat {

    private def error (msg: String) : Nothing = throw new IllegalArgumentException (msg)

    def apply[Y <: Int, X <: Int, T] (elems: T*) (implicit y: ValueOf[Y], x: ValueOf[X]) : Mat[Y, X, T] = {
        val dimY = y.value
        val dimX = x.value
        if (dimY <= 0 || dimX <= 0) error ("`dimY` and `dimX` must be strictly positive")
        if (elems.size != dimY * dimX) error (s"Expected ${dimY * dimX} elements, found ${elems.size}")
        new Mat[Y, X, T] (elems.toSeq)
    }

    implicit def mkField[D <: Int, T] (implicit d: ValueOf[D], field: Field[T]) : Field[Mat[D, D, T]] = {
        val dim = d.value
        val size = dim * dim
        type Self = Mat[D, D, T]
        def toCoords (idx: Int) : (Int, Int) = (idx / dim, idx % dim)
        new Field[Self] {
            override def plus (x: Self, y: Self) : Self = new Mat ((x.elems zip y.elems) .map {case (a, b) => a + b})
            override def times (x: Self, y: Self) : Self = new Mat (
                (0 until dim) .map { idx =>
                    val (j, i) = toCoords (idx)
                    (0 until dim) .foldLeft (field.zero) {
                        case (partial, k) => partial + x (j, k) * y (k, i)
                    }
                }
            )
            override def negate (x: Self) : Self = new Mat (x.elems.map (-_))
            override def invert (x: Self): Self = ???
            override lazy val zero: Self = new Mat (Seq.fill (size) (field.zero))
            override lazy val one: Self = new Mat (
                (0 until dim) .map {
                    toCoords (_) match {
                        case (j, i) if (j == i) => field.one
                        case _ => field.zero
                    }
                }
            )
        }
    }

}