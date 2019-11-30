package com.github.nhweston.grp.data

class Mat[D <: Int, T] private (val elems: IndexedSeq[T]) (implicit d: ValueOf[D]) {

    def dim: Int = d.value

    def toIndex (j: Int, i: Int) : Int = {
        if (j < 0 || j >= dim || i < 0 || i >= dim) throw new IndexOutOfBoundsException (s"($j, $i)")
        else i + j * dim
    }

    def toCoords (index: Int) : (Int, Int) = {
        if (index < 0 || index > elems.length) throw new IndexOutOfBoundsException (s"$index")
        else (index / dim, index % dim)
    }

    def apply (j: Int, i: Int) : T = elems (toIndex (j, i))

    def column (i: Int) : Iterator[T] = new Iterator[T] {
        private var count = 0
        private var index = toIndex (0, i)
        override def hasNext: Boolean = count < dim
        override def next () : T = {
            val result = elems (index)
            count += 1
            index += dim
            result
        }
    }

    def row (j: Int) : Iterator[T] = new Iterator[T] {
        private var count = 0
        private var index = toIndex (j, 0)
        override def hasNext: Boolean = count < dim
        override def next () : T = {
            val result = elems (index)
            count += 1
            index += 1
            result
        }
    }

    override lazy val toString: String = {
        val strings = elems.map (_.toString)
        val widths = (0 until dim) .map (i => (0 until dim) .map (j => (strings (toIndex (j, i)) .length)) .max)
        val padded = elems.zipWithIndex.map {
            case (str, idx) => s"%${widths (toCoords (idx) ._1) + 1}s" .format (str)
        }
        val builder = new StringBuilder ()
        for (j <- 0 until dim) {
            builder += '\n'
            for (i <- 0 until dim) builder ++= padded (toIndex (j, i))
        }
        builder.result ()
    }

    override def equals (o: Any) : Boolean = {
        o match {
            case other: Mat[D, T] => (this.elems zip other.elems) .forall {case (a, b) => a == b}
            case _ => false
        }
    }

    override def hashCode: Int = elems.map (_.hashCode) .toArray.hashCode

}

object Mat {

    private def error (msg: String) : Nothing = throw new IllegalArgumentException (msg)

    def apply[D <: Int, T] (elems: T*) (implicit d: ValueOf[D]) : Mat[D, T] = {
        val dim = d.value
        if (dim <= 0) error ("`dimY` and `dimX` must be strictly positive")
        if (elems.size != dim * dim) error (s"Expected ${dim * dim} elements, found ${elems.size}")
        new Mat[D, T] (IndexedSeq from elems)
    }

}
