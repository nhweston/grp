package com.github.nhweston.grp

import com.github.nhweston.grp.data.Mod
import shapeless.ops.hlist.RightFolder
import shapeless.{::, HList, HNil, Poly, Poly1, Poly2}

case class Pres[H, T <: HList](
    sub: (T, T) => T,
    act: (H, T) => T,
    op: H => H,
    condense: H => (H, T),
) {

    def combine(x: H :: T, y: H :: T): H :: T = {
        (x, y) match {
            case (xh :: xt, yh :: yt) =>
                val t = act(xh, sub(xt, yt))
                val h =
        }
    }

    object combine extends Poly2 {
        implicit def combine: ProductCase.Aux[H :: T, H :: T] = use {
            (x: H :: T, y: H :: T) => {
                act
            }
        }
    }

}

object Pres {

    case class FCons[H <: Int, T: Pres](act: T => T, condense: T) {

        def act()

    }

    object FNil extends Pres

}

object Test {

    class Validate[E] {
        def apply[L <: HList] (hlist: L) (implicit folder: RightFolder[L, Either[List[E], HNil], combine.type]) =
            hlist.foldRight (Right (HNil) : Either[List[E], HNil]) (combine)
    }

    object combine extends Poly2 {
        implicit def combine[E, H, T <: HList]
        : ProductCase.Aux[Either[E, H] :: Either[List[E], T] :: HNil, Either[List[E], H :: T]] = use {
            (elem: Either[E, H], result: Either[List[E], T]) => (elem, result) match {
                case (Left (error), Left (errors)) => Left (error :: errors)
                case (Left (error), Right (_)) => Left (error :: Nil)
                case (Right (_), Left (errors)) => Left (errors)
                case (Right (value), Right (values)) => Right (value :: values)
            }
        }
    }

    def validate[E] = new Validate[E]


}
