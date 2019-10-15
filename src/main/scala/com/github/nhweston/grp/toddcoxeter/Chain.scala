package com.github.nhweston.grp.toddcoxeter

import com.github.nhweston.grp.toddcoxeter.ToddCoxeter.{Generator, Label}

case class Chain (left: Label, gtors: Seq[Generator], right: Label) (implicit val id: Int) {

    def reduce (links: LinkTable) : Chain = {
        gtors match {
            case _ +: Nil => this
            case head +: tail if (links.left contains (left, head)) =>
                Chain (links.left ((left, head)), tail, right) .reduce (links)
            case init :+ last if (links.right contains ((right, last))) =>
                Chain (left, init, links.right ((right, last))) .reduce (links)
            case _ => this
        }
    }

    def merge (l1: Label, l2: Label) : Chain = {
        copy (
            left = if (left == l2) l1 else left,
            right = if (right == l2) l1 else right
        )
    }

    override def toString: String = {
        gtors.foldLeft (s"$left ") {
            case (str, gtor) => s"$str${gtor.toChar}"
        } + s"$right"
    }

}

object Chain {

    sealed abstract class ReductionResult
    case class Unchanged (existing: Chain) extends ReductionResult
    sealed abstract class Changed extends ReductionResult
    case class ReducedToChain (chain: Chain) extends Changed
    case class ReducedToLink (left: Label, gtor: Generator, right: Label) extends Changed

}
