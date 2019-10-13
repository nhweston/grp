package com.github.nhweston.grp.model

object Lin {

    type Lin[D <: Int, M <: Int] = Mat[D, D, Mod[M]]

    def Lin[D <: Int, M <: Int] (entries: Int*) (implicit d: ValueOf[D], m: ValueOf[M]) : Lin[D, M] = {
        Mat[D, D, Mod[M]] (entries.map (Mod[M] (_)) :_*)
    }

}
