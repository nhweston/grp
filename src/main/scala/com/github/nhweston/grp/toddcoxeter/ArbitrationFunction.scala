package com.github.nhweston.grp.toddcoxeter

import com.github.nhweston.grp.toddcoxeter.ToddCoxeter.{Generator, Label}

import scala.collection.SortedSet
import scala.util.Random

object ArbitrationFunction {

    type ArbitrationFunction = ((SortedSet[Chain], LinkTable) => (Label, Generator))

    val ShortestChain: ArbitrationFunction = {
        (chains, _) => {
            chains.minBy (_.gtors.size) match {
                case Chain (left, head +: _, _) => (left, head)
            }
        }
    }

    val Random: ArbitrationFunction = {
        val rng: Random = new Random ()
        (_, links) => {
            val seq = links.gaps.toSeq
            seq (rng.nextInt (seq.size))
        }
    }

}
