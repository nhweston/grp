package com.github.nhweston.grp.morph

import com.github.nhweston.grp.FinGrp

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Decides whether two finite groups are isomorphic. This algorithm proceeds by finding a set of generators for `G`,
 * then attempts to identify each of these generators to an element of `H` à la brute force. At each step, the
 * identifications made so far will be validated. Backtracking occurs whenever a contradiction arises.
 */
case class Isomorphism[G, H] (
    G: FinGrp[G],
    H: FinGrp[H]
) {

    lazy val gtorsG: Seq[G] = G.findGtors.toSeq
    lazy val tableG: Seq[G] = G.zero +: (G.set - G.zero) .toSeq
    lazy val idxsG: Map[G, Int] = tableG.zipWithIndex.toMap

    lazy val result: Option[Partial] = {
        /** Attempts to complete a partial set of identifications between generators of G and H. */
        def search (result: Partial) : Option[Partial] = {
            @tailrec
            def aux (candidates: Seq[H]) : Option[Partial] = {
                candidates match {
                    case candidate +: candidatesNext =>
                        result + candidate match {
                            case Some (result) => search (result)
                            case None => aux (candidatesNext)
                        }
                    case Nil => None
                }
            }
            if (result.gtorsH.size < gtorsG.size) aux (H.set.toSeq)
            else Some (result)
        }
        search (Partial (Some (H.zero) +: Seq.fill (H.set.size - 1) (None), Seq.empty))
    }

    /**
     * Represents a partial set of identifications between elements of G and H. If all options in `tableH` are defined,
     * then this is a complete isomorphism, thus `G` and `H` are isomorphic.
     *
     * @param tableH    the option at index i contains the element of `H` identified with the element of `G` in
     *                  `tableG` at index `i`, if any.
     * @param gtorsH    the element at index `i` is the element of `H` identified with the element of `G` in `gtorsG`
     *                  at index `i`.
     */
    case class Partial (
        tableH: Seq[Option[H]],
        gtorsH: Seq[H]
    ) {

        /**
         * Identifies the element of `G` in `gtorsG` at index `gtorsH.size` with `h` and reconstructs the set of
         * identifications to account for this. If this leads to a contradiction, `None` is returned. Otherwise, `Some`
         * of the resulting `Partial` is returned.
         */
        def + (gtorH: H) : Option[Partial] = {
            val gtors: Seq[(G, H)] = gtorsG zip (gtorsH :+ gtorH)
            @tailrec
            def aux (
                q: Queue[(G, H)],
                result: Option[Partial]
            ) : Option[Partial] = (q, result) match {
                case (Queue (), _) | (_, None) => result
                case ((xG, xH) +: tail, Some (partial)) => partial.tableH (idxsG (xG)) match {
                    // Case 1: Agrees with an existing identification.
                    case Some (`xH`) => aux (tail, result)
                    // Case 2: A new identification.
                    case None if ! (tableH contains (Some (xH))) =>
                        // What new identifications does this lead to?
                        val gted = gtors.map {case (gtorG, gtorH) => (G.plus (xG, gtorG), H.plus (xH, gtorH))}
                        val next = Partial (partial.tableH.updated (idxsG (xG), Some (xH)), gtorsH)
                        aux (tail ++ gted, Some (next))
                    // Case 3: Contradiction.
                    case _ => None
                }
            }
            aux (
                // Find all new identifications resulting immediately from existing ones and the new generator.
                Queue from {
                    val existing = tableH.zipWithIndex.collect {case (Some (_), idx) => idx}
                    for (idx <- existing) yield {
                        val (gtorG, gtorH) = gtors.last
                        val xG = tableG (idx)
                        val xH = tableH (idx) .get
                        val nextG = G.plus (xG, gtorG)
                        val nextH = H.plus (xH, gtorH)
                        nextG -> nextH
                    }
                },
                Some (Partial (tableH, gtorsH :+ gtorH))
            )
        }

    }

}
