package com.github.nhweston.grp.toddcoxeter

import com.github.nhweston.grp.FinGrp
import com.github.nhweston.grp.toddcoxeter.ArbitrationFunction._
import com.github.nhweston.grp.toddcoxeter.ToddCoxeter.{Label, Link}

import scala.annotation.tailrec
import scala.collection.immutable.{ArraySeq, SortedSet}

class ToddCoxeter (
    val presentation: Presentation,
    val arbitrationFn: ArbitrationFunction = ShortestChain
) {

    private var stepCount: Int = 0
    private var chainCount: Int = 0

    implicit def nextChainId: Int = {
        val result = chainCount
        chainCount += 1
        result
    }

    implicit val ordChain: Ordering[Chain] = Ordering by (_.id)

    case class Symbol (string: String) (val label: Label) {
        override val toString: String = label.toString
    }

    lazy val cayley: CayleyBuilder = {
        @tailrec
        def aux (
            labels: Set[Label],
            chains: SortedSet[Chain],
            links: LinkTable,
            counter: Label
        ) : CayleyBuilder = {
            println (s"Step $stepCount")
            stepCount += 1
            val (chainsNext, deductions) = {
                // TODO: Multi-thread this!
                chains.foldLeft ((SortedSet.empty[Chain], Set.empty[Link])) {
                    case ((cn, d), chain) => chain.reduce (links) match {
                        case Chain (l, gtor +: Nil, r) => (cn, d + ((l, gtor, r)))
                        case reduced @ Chain (_, _, _) => (cn + reduced, d)
                    }
                }
            }
            val linksNext = links ++ deductions.toSeq
            val coincidences = linksNext.coincidences
            val toRemove: Set[Label] = labels intersect coincidences.keySet
            if (toRemove.isEmpty) {
                if (deductions.isEmpty) {
                    if (links.gaps.isEmpty) {
                        println ("Finished.")
                        new CayleyBuilder (
                            presentation.generators.toSeq,
                            labels.toSeq,
                            linksNext
                        )
                    }
                    else {
                        println ("No deductions, arbitration required.")
                        val (l, gtor) = arbitrationFn (chainsNext, linksNext)
                        aux (
                            labels + counter,
                            chains ++ presentation.relations.map (Chain (counter, _, counter)),
                            linksNext.arbitrate (counter) + (l, gtor, counter),
                            counter + 1
                        )
                    }
                }
                else {
                    println ("Proceeding to another round of deductions.")
                    aux (labels, chainsNext, linksNext, counter)
                }
            }
            else {
                println ("Coincidence found.")
                aux (
                    labels -- coincidences.keys,
                    chainsNext.map {
                        case Chain (l, gtor, r) =>
                            val left = coincidences.getOrElse (l, l)
                            val right = coincidences.getOrElse (r, r)
                            Chain (left, gtor, right)
                    },
                    linksNext,
                    counter
                )
            }
        }
        aux (
            Set (0),
            SortedSet from presentation.relations.map (Chain (0, _, 0)),
            LinkTable (
                presentation.generators,
                Set (0),
                Map.empty,
                Map.empty,
                SortedSet from presentation.generators.map ((0, _)),
                Map.empty
            ),
            1
        )
    }

    lazy val toGrp: FinGrp[Int] = new FinGrp[Label] {
        override def set: Set[Label] = cayley.paths.keySet
        override def op: (Label, Label) => Label = (x, y) => cayley.result ((x, y))
    }

}

object ToddCoxeter {

    def apply (relations: String*) : ToddCoxeter = {
        new ToddCoxeter (Presentation (relations :_*))
    }

    type Generator = Byte
    type Relation = ArraySeq[Byte]
    type Label = Int
    type Link = (Label, Generator, Label)

}
