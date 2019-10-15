package com.github.nhweston.grp.toddcoxeter

import com.github.nhweston.grp.toddcoxeter.ToddCoxeter.{Generator, Label}

import scala.collection.immutable.Queue

class CayleyBuilder (gtors: Seq[Generator], labels: Seq[Label], links: LinkTable) {

    lazy val paths: Map[Label, Seq[Generator]] = {
        def aux (
            elem: Label,
            prod: Queue[Generator],
            result: Map[Label, Seq[Generator]]
        ) : Map[Label, Seq[Generator]] = {
            if (result contains elem) result
            else gtors.foldLeft (result + (elem -> prod)) {
                case (next, gtor) => aux (links.left ((elem, gtor)), prod :+ gtor, next)
            }
        }
        aux (0, Queue.empty, Map.empty)
    }

    def compute (l1: Label, l2: Label) : Label = {
        paths (l2) .foldLeft (l1) {
            case (l, gtor) => links.left ((l, gtor))
        }
    }

    lazy val result: Map[(Label, Label), Label] = {
        labels.flatMap {
            l1 => labels.map {
                l2 => (l1, l2) -> compute (l1, l2)
            }
        } .toMap
    }

}
