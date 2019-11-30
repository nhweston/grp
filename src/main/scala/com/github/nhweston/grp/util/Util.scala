package com.github.nhweston.grp.util

object Util {

    def join[T] (x: T, n: Int) (f: (T, T) => T) : T = {
        def aux (x: T, n: Int) : T = {
            if (n == 1) x
            else {
                val sqrt = aux (x, n / 2)
                val sqrd = f (sqrt, sqrt)
                if (n % 2 == 0) sqrd
                else f (sqrd, x)
            }
        }
        if (n <= 0) throw new IllegalArgumentException
        aux (x, n)
    }

}
