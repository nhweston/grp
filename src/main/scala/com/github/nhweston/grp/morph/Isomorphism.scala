package com.github.nhweston.grp.morph

import com.github.nhweston.grp.FinGrp

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Isomorphism[G, H] (
    G: FinGrp[G],
    H: FinGrp[H],
    gtorsG: Seq[G]
) {

    lazy val tableG: Seq[G] = G.zero +: (G.set - G.zero) .toSeq
    lazy val idxsG: Map[G, Int] = tableG.zipWithIndex.toMap

    lazy val result: Option[Partial] = {
        def search (result: Option[Partial]) : Option[Partial] = {
            result match {
                case Some (Partial (tableH, gtorsH)) =>
                    @tailrec
                    def aux (candidates: Seq[H]) : Option[Partial] = {
                        candidates match {
                            case candidate +: candidatesNext =>
                                Partial (tableH, gtorsH :+ candidate) .regenerate match {
                                    case result @ Some (_) => search (result)
                                    case None => aux (candidatesNext)
                                }
                            case Nil => None
                        }
                    }
                    if (gtorsH.size < gtorsG.size) aux (H.set.toSeq)
                    else result
                case None => None
            }
        }
        search (Some (Partial (Some (H.zero) +: Seq.fill (H.set.size - 1) (None), Seq.empty)))
    }

    case class Partial (
        tableH: Seq[Option[H]],
        gtorsH: Seq[H]
    ) {

        lazy val gtors: Seq[(G, H)] = gtorsG zip gtorsH

        def regenerate: Option[Partial] = {
            @tailrec
            def aux (
                q: Queue[(G, H)],
                result: Option[Partial]
            ) : Option[Partial] = (q, result) match {
                case (Queue (), _) | (_, None) => result
                case ((xG, xH) +: tail, Some (partial)) => partial.tableH (idxsG (xG)) match {
                    case Some (`xH`) => aux (tail, result)
                    case None if tableH.forall {
                        case Some (`xH`) => false
                        case _ => true
                    } =>
                        val gted = gtors.map {case (gtorG, gtorH) => (G.plus (xG, gtorG), H.plus (xH, gtorH))}
                        val next = Partial (partial.tableH.updated (idxsG (xG), Some (xH)), gtorsH)
                        aux (tail ++ gted, Some (next))
                    case _ => None
                }
            }
            val existing = tableH.zipWithIndex.collect {case (Some (_), idx) => idx}
            aux (
                Queue from {
                    for (idx <- existing; (gtorG, gtorH) <- gtors) yield {
                        val xG = tableG (idx)
                        val xH = tableH (idx) .get
                        val nextG = G.plus (xG, gtorG)
                        val nextH = H.plus (xH, gtorH)
                        nextG -> nextH
                    }
                },
                Some (this)
            )
        }
    }

}
