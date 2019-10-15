package com.github.nhweston.grp.toddcoxeter

import com.github.nhweston.grp.toddcoxeter.ToddCoxeter.{Generator, Label, Link}

import scala.collection.immutable.SortedSet

case class LinkTable (
    gtors: Set[Generator],
    labels: Set[Label],
    left: Map[(Label, Generator), Label],
    right: Map[(Label, Generator), Label],
    gaps: SortedSet[(Label, Generator)],
    coincidences: Map[Label, Label]
) {

    def + (deduction: Link) : LinkTable = {
        val (lL, gtor, lR) = deduction
        val l = coincidences.getOrElse (lL, lL)
        val r = coincidences.getOrElse (lR, lR)
        left.get ((l, gtor)) match {
            case Some (other) =>
                if (other == r) this
                else {
                    println (s"$l${gtor.toChar}$r contradicts $l${gtor.toChar}$other")
                    merge (other, r)
                }
            case None =>
                println (s"$l${gtor.toChar}$r")
                copy (
                    left = left + ((l, gtor) -> r),
                    right = right + ((r, gtor) -> l),
                    gaps = gaps - ((l, gtor))
                )
        }
    }

    def ++ (deductions: Seq[Link]) : LinkTable = {
        deductions match {
            case head +: tail => (this + head) ++ tail
            case Nil => this
        }
    }

    def merge (label1: Label, label2: Label) : LinkTable = {
        val swap = label1 > label2
        val l1 = if (swap) label2 else label1
        val l2 = if (swap) label1 else label2
        val toRemove = {
            left.collect[Link] {
                case ((l, gtor), r) if (l == l2 || r == l2) => (l, gtor, r)
            } .toSet
        }
        val toAdd = {
            toRemove.map {
                case (l, gtor, r) => (if (l == l2) l1 else l, gtor, if (r == l2) l1 else r)
            } .toSeq
        }
        LinkTable (
            gtors,
            labels - l2,
            left.filterNot {
                case ((l, gtor), r) => toRemove contains (l, gtor, r)
            },
            right.filterNot {
                case ((r, gtor), l) => toRemove contains (l, gtor, r)
            },
            gaps.filterNot (_._1 == l2),
            (coincidences + (l2 -> l1)) .map {
                case (l, `l2`) => (l, l1)
                case c => c
            }
        ) ++ toAdd
    }

    def arbitrate (label: Label) : LinkTable = {
        println (s"New label $label")
        copy (
            labels = labels + label,
            gaps = gaps ++ gtors.map ((label, _))
        )
    }

}
