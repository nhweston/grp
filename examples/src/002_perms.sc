// Defining S7.

import com.github.nhweston.grp
import grp._
import grp.model._
import Grp._
import Perm._

// Our group elements are Perm[7].
type G = Perm[7]

// Summoning the `Grp` instance:
val G: Grp[G] = implicitly

// We can use `cyc[7]` to define a single cycle via cycle notation, but we will abbreviate this to just `C`.
def C (cycle: Int*) : G = cyc[7] (cycle :_*)

// Try it:
C (0,1,2)
C (2,4,3,5,6)

// Let's try out the group operator:
C(0,1) + C(2,3)
C(2,4,6) + C(3,4,5,6)
C(0,1,2,3,4,5,6) * 3
- (C(1,2,3) + C(4,5))

// The orders of elements:
G.order (C(0,1,2,3,4,5,6))
G.order (C(0,1,2))
G.order (C(0,1,2) + C(3,4))

// All elements of a symmetry group can be generated from a simple transposition and a full cycle. We'll generate S6
// here since S7 is a little too computationally expensive:
val S6 = G.generate (C(0,1), C(0,1,2,3,4,5))
S6.set.size

// An order profile of S6:
S6.orderProfile

// All finite groups are a subgroup of some symmetry group, so there's a lot of interesting groups we can find.

// C7:C3 (the smallest non-Abelian group of odd order)
val C7C3 = G.generate (C(0,1,2,3,4,5,6), C(1,2,4)+C(3,6,5))
C7C3.set
C7C3.orderProfile

// GL(3,2)
val GL3_2 = G.generate (C(0,1)+C(3,4), C(1,2,3)+C(4,5,6))
GL3_2.set.size
GL3_2.orderProfile
