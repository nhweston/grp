import com.github.nhweston.grp
import grp._
import grp.model._
import Grp._
import Mat._
import Mod._
import Lin._
import grp.util.Table
import grp.morph.Isomorphism

def example[G, H] (G: FinGrp[G], H: FinGrp[H]) : Table[Int, String, Any] = {
    val morph = Isomorphism (G, H, G.findGtors.toSeq)
    val result = morph.result.get
    Table (
        (0 until G.set.size), Seq ("G", "H"),
        (idx: Int, label: String) => {
            label match {
                case "G" => morph.tableG (idx)
                case "H" => result.tableH (idx) .get
            }
        }
    )
}

object Example01 {

    // D6 is isomorphic to S3.

    type G = (Mod[3], Mod[2])
    val G: FinGrp[G] = FinGrp.genToGrp[G] (
        (Mod[3](0), Mod[2](0)),
        Set (
            (Mod[3](1), Mod[2](0)),
            (Mod[3](0), Mod[2](1))
        ),
        (x, y) => (
            if (x._2.n == 1) x._1 - y._1 else x._1 + y._1,
            x._2 + y._2
        )
    )

    type H = Perm[3]
    val H: FinGrp[H] = FinGrp.genToGrp[H] (
        Perm.zero,
        Set (Perm.sigma, Perm.tau),
        _ + _
    )

    val result = example (G, H)

}

object Example02 {

    // Isomorphism of GL(2,3) and a permutation representation:
    // https://people.maths.bris.ac.uk/~matyd/GroupNames/1/GL(2,3).html

    type G = Perm[8]
    def a (cycle: Int*) : Perm[8] = Perm.cyc[8] (cycle :_*)
    val G = FinGrp.genToGrp[Perm[8]] (
        Perm.zero[8],
        Set (
            a(0,1,2,3) + a(4,5,6,7),
            a(0,4,2,6) + a(1,7,3,5),
            a(1,4,7) + a(3,6,5),
            a(0,2) + a(1,4) + a(3,6)
        ),
        _ + _
    )

    type H = Lin[2,3]
    val evH: Grp[Lin[2,3]] = implicitly
    def b (entries: Int*) : Lin[2,3] = Lin (entries :_*)
    val H = FinGrp.genToGrp[Lin[2,3]] (
        evH.zero,
        Set (
            b(2,1,1,1),
            b(1,1,1,2),
            b(1,2,0,1),
            b(2,0,0,1)
        ),
        _ + _
    )

    val result = example (G, H)

}

object Example03 {

    // Isomorphism of GL(2,3) and a permutation representation:
    // https://people.maths.bris.ac.uk/~matyd/GroupNames/480/GL(2,5).html

    type G = Perm[24]
    def g (cycle: Int*) = Perm.cyc[24] (cycle :_*)
    val G = FinGrp.genToGrp[G] (
        Perm.zero,
        Set (
            Perm.sigma,
            g(0,4,1,11) + g(2,8,14,20) + g(5,18,22,19) + g(6,10,7,17) + g(12,16,13,23)
        ),
        _ + _
    )
    type H = Lin[2,5]
    val evH: Grp[Lin[2,5]] = implicitly
    val H = FinGrp.genToGrp[H] (
        evH.zero,
        Set (
            Lin(1,4,2,1),
            Lin(0,3,4,1)
        ),
        _ + _
    )

    val result = example (G, H)

}
