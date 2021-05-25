type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

exception Error of string
let rec crazy2add ((x : crazy2), (y: crazy2)) =

if x=NIL then raise (Error "Invalid input : NIL")
else if y=NIL then raise (Error "Invalid input : NIL")
else

   let rec tempadd (x, y) =
     match (x,y) with
         (NIL, NIL) -> NIL
        |(NIL, ZERO(b)) -> ZERO(tempadd(NIL,b))
        |(NIL, ONE(b)) -> ONE(tempadd(NIL,b))
        |(NIL, MONE(b)) -> MONE(tempadd(NIL,b))

        |(ZERO(a), ZERO(c)) -> ZERO(tempadd(a,c))
        |(ZERO(a), ONE(c)) -> ONE(tempadd(a,c))
        |(ZERO(a), MONE(c)) -> MONE(tempadd(a,c))

        |(ONE(a), ONE(b)) -> ZERO(tempadd((tempadd(a,b)), (ONE NIL)) )
        |(ONE(a), MONE(b)) -> ZERO(tempadd(a,b))

        |(MONE(a), MONE(b)) -> ZERO(tempadd((tempadd(a,b)), (MONE NIL)) ) 
        |_ -> tempadd(y,x)
   in

   let rec checkZERO (x) =
     match x with
         ZERO NIL -> ZERO NIL
        |ZERO(a) -> if checkZERO(a)= ZERO NIL then ZERO NIL
                           else ZERO(a)
        |ONE(a) -> ONE(a)
        |MONE(a) -> MONE(a)
        |NIL -> NIL
   in

checkZERO(tempadd(x, y));;