package com.github.nhweston.grp.toddcoxeter

import com.github.nhweston.grp.toddcoxeter.ToddCoxeter.{Generator, Relation}

import scala.collection.immutable.ArraySeq

class Presentation (val relations: Seq[Relation]) {

    lazy val generators: Set[Generator] = {
        val builder = Set.newBuilder[Generator]
        for (rel <- relations; gtor <- rel) builder += gtor
        builder.result ()
    }

}

object Presentation {

    def apply (relations: String*) : Presentation = {
        new Presentation (
            relations.map {
                rel => {
                    val builder = ArraySeq.newBuilder[Byte]
                    for (gtor <- rel) builder += gtor.toByte
                    builder.result ()
                }
            } .toSeq
        )
    }

}
