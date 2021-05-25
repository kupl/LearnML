(*2-2 컴공 2014-10618 이세영*)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;
let rec addThree (a, b, c)=
    match (a,b,c) with
    |(NIL, NIL, -1)->MONE NIL
    |(NIL, NIL, 0)->NIL
    |(NIL, NIL, 1)->ONE NIL
    |(NIL, MONE x, -1)->ZERO(addThree(x,NIL,-1))
    |(NIL, MONE x, 0)->MONE x
    |(NIL, MONE x, 1)->ZERO x
    |(NIL, ZERO x, -1)->MONE x
    |(NIL, ZERO x, 0)->ZERO x
    |(NIL, ZERO x, 1)->ONE x
    |(NIL, ONE x, -1)->ZERO x
    |(NIL, ONE x, 0)->ONE x
    |(NIL, ONE x, 1)->ZERO(addThree(x,NIL,1))
    |(ONE z, NIL, -1)->ZERO z
    |(ONE z, NIL, 0)->ONE z
    |(ONE z, NIL, 1)->ZERO(addThree(z,NIL,1))
    |(ONE z, MONE x, -1)->MONE(addThree(z,x,0))
    |(ONE z, MONE x, 0)->ZERO(addThree(z,x,0))
    |(ONE z, MONE x, 1)->ONE(addThree(z,x,0))
    |(ONE z, ZERO x, -1)->ZERO(addThree(z,x,0))
    |(ONE z, ZERO x, 0)->ONE(addThree(z,x,0))
    |(ONE z, ZERO x, 1)->ZERO(addThree(z,x,1))
    |(ONE z, ONE x, -1)->ONE(addThree(z,x,0))
    |(ONE z, ONE x, 0)->ZERO(addThree(z,x,1))
    |(ONE z, ONE x, 1)->ONE(addThree(z,x,1))
    |(ZERO z, NIL, -1)->MONE z
    |(ZERO z, NIL, 0)->ZERO z
    |(ZERO z, NIL, 1)->ONE z
    |(ZERO z, MONE x, -1)->ZERO(addThree(z,x,-1))
    |(ZERO z, MONE x, 0)->MONE(addThree(z,x,0))
    |(ZERO z, MONE x, 1)->ZERO(addThree(z,x,0))
    |(ZERO z, ZERO x, -1)->MONE(addThree(z,x,0))
    |(ZERO z, ZERO x, 0)->ZERO(addThree(z,x,0))
    |(ZERO z, ZERO x, 1)->ONE(addThree(z,x,0))
    |(ZERO z, ONE x, -1)->ZERO(addThree(z,x,0))
    |(ZERO z, ONE x, 0)->ONE(addThree(z,x,0))
    |(ZERO z, ONE x, 1)->ZERO(addThree(z,x,1))
    |(MONE z, NIL, -1)->ZERO(addThree(z,NIL,-1))
    |(MONE z, NIL, 0)->MONE z
    |(MONE z, NIL, 1)->ZERO z
    |(MONE z, MONE x, -1)->MONE(addThree(z,x,-1))
    |(MONE z, MONE x, 0)->ZERO(addThree(z,x,-1))
    |(MONE z, MONE x, 1)->MONE(addThree(z,x,0))
    |(MONE z, ZERO x, -1)->ZERO(addThree(z,x,-1))
    |(MONE z, ZERO x, 0)->MONE(addThree(z,x,0))
    |(MONE z, ZERO x, 1)->ZERO(addThree(z,x,0))
    |(MONE z, ONE x, -1)->MONE(addThree(z,x,0))
    |(MONE z, ONE x, 0)->ZERO(addThree(z,x,0))
    |(MONE z, ONE x, 1)->ONE(addThree(z,x,0))
let crazy2add (c1, c2) =
    addThree(c1, c2, 0);;
