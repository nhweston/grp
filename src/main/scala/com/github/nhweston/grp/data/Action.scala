package com.github.nhweston.grp.data

case class Action[T] (f: T => T) (val inv: T => T) extends (T => T) {

    override def apply (t: T) : T = f (t)

}
