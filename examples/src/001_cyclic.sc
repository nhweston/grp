// Defining C12.

import com.github.nhweston.grp
import grp._
import grp.model._
import Grp._
import Mod._

// Our group elements are Mod[12].
type G = Mod[12]

// To simplify having to type `Mod[12](n)` for a group element, we will define:
def a (n: Int) : G = Mod[12] (n)

// Scala's implicit voodoo means the compiler has already knows how to define a group over G. Try it:
a(1) + a(2)
a(9) + a(7)

// We can compute inverses...
-a(4)
a(5) - a(3)

// ...and multiply:
a(2) * 5
a(7) * 11

// We will summon the `Grp` instance itself so we can access some more operations:
val G: Grp[G] = implicitly

// The identity element:
G.zero

// Finding orders of elements:
G.abs (a(1))
G.abs (a(6))
G.abs (a(11))

// We can also generate subgroups:
val H2 = G.generate (a(3))
H2.set
