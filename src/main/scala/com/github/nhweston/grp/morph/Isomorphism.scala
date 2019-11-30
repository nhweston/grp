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
//                    if (gtorsH.size < gtorsG.size) aux ((H.set -- tableH.flatten) .toSeq)
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
//            aux (
//                Queue from (tableG zip tableH) .collect {case (xG, Some (xH)) => xG -> xH},
//                Some (Partial (Seq.fill (H.set.size) (None), gtorsH))
//            )
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

object Isomorphism {

//    case class Partial[G, H] (
//        G: FinGrp[G],
//        H: FinGrp[H],
//        gtors: Seq[(G, H)],
//        elemsByIdx: Map[G, Int],
//        tableG: Seq[G],
//        tableH: Seq[Option[H]]
//    ) {
//
//        def insertGtorPair (pair: (G, H)) : Option[Partial[G, H]] = {
//            val (gtorG, gtorH) = pair
//            val updated = copy (gtors = gtors :+ pair)
//            @tailrec
//            def aux (
//                elems: Seq[(G, Int)],
//                result: Option[Partial[G, H]]
//            ) : Option[Partial[G, H]] = {
//                (elems, result) match {
//                    case (Nil, _) | (_, None) => result
//                    case ((xG, idx) +: tail, Some (result)) =>
//                        tableH (idx) match {
//                            case Some (xH) =>
//                                val nextG = G.append (xG, gtorG)
//                                val nextH = H.append (xH, gtorH)
//                                aux (tail, result + (nextG -> nextH))
//                            case None => aux (tail, Some (result))
//                    }
//                }
//            }
//            aux (elemsByIdx.toSeq, Some (updated))
//        }
//
//        def + (entry: (G, H)) : Option[Partial[G, H]] = {
//            val (xG, xH) = entry
//            val idx = elemsByIdx (xG)
//            tableH (idx) match {
//                case Some (existing) =>
//                    if (existing == xH) Some (this)
//                    else None
//                case None =>
//                    @tailrec
//                    def aux (
//                        gtors: Seq[(G, H)],
//                        result: Option[Partial[G, H]],
//                    ) : Option[Partial[G, H]] = {
//                        (gtors, result) match {
//                            case (Nil, _) | (_, None) => result
//                            case (((gtorG, gtorH) +: tail), Some (result)) =>
//                                val nextG = G.append (xG, gtorG)
//                                val nextH = H.append (xH, gtorH)
//                                val updated = result.copy (tableH = tableH.updated (elemsByIdx (nextG), Some (nextH)))
//                                aux (tail, updated + (nextG -> nextH))
//                        }
//                    }
//                    aux (gtors, Some (this))
//            }
//        }
//
//    }
//
//    def isIsomorphic[G, H] (G: FinGrp[G], H: FinGrp[H]) : Boolean = {
//        val gtors = G.findGtors
//        def search (
//            partial: Option[Partial[G, H]],
//            gtorsRemaining: Seq[G]
//        ) : Option[Partial[G, H]] = {
//            (partial, gtorsRemaining) match {
//                case (None, _) => None
//                case (Some (partial), Nil) => if (partial.tableH.forall (_.isDefined)) Some (partial) else None
//                case (Some (partial), gtor +: gtorsRemaining) =>
//                    @tailrec
//                    def aux (candidates: Seq[H]) : Option[Partial[G, H]] = {
//                        candidates match {
//                            case candidate +: candidatesNext =>
//                                partial.insertGtorPair (gtor -> candidate) match {
//                                    case result @ Some (_) => search (result, gtorsRemaining)
//                                    case None => aux (candidatesNext)
//                                }
//                            case Nil => None
//                        }
//                    }
//                    aux ((H.set -- partial.tableH.flatten) .toSeq)
//            }
//        }
//        ???
//        search {
//            val tableG = G.zero +: (G.set - G.zero) .toSeq
//            val tableH = Seq (Some (H.zero)) ++ Seq.fill (H.set.size - 1) (None)
//            val elemsByIdx =
//        }
//        search (
//            Some (Partial (
//                G, H, Seq.empty,
//                (G.set - G.zero)
//            ))
//        )
//    }

}
