// Defining GL(2,3).

import com.github.nhweston.grp
import grp._
import grp.model._
import Grp._
import Mat._
import Mod._
import Lin._

// GL(2,3) is the group of all 2-by-2 invertible matrices over the finite field of order 3. Broadly speaking, the
// finite field of order 3 can be thought of as simply integers modulo 3.

type G = Lin[3, 3]

// Let's define a shorter notation for these matrices:
def G (
    a00: Int, a01: Int,
    a10: Int, a11: Int
) : G = Lin[3, 3] (a00, a01, a10, a11)

// Try it:
G (1, 1, 2, 0)

// As usual we use `+` for the group operator, but note that the group operator is matrix multiplication.
G (1, 1, 2, 0) + G (2, 1, 2, 0)

val G: Grp[G] = implicitly
