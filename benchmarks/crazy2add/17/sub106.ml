(*real code start*)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (c : crazy2) : int = 
match c with
|NIL ->0
|ZERO(x) -> 2*crazy2val(x)
|ONE(x) -> 2*crazy2val(x) +1
|MONE(x) -> 2*crazy2val(x) -1

let rec inside (c:crazy2) : crazy2 =
match c with
|NIL->NIL
|ZERO(x)|ONE(x)|MONE(x)->x

let rec c2carry ((c1: crazy2), (c2:crazy2)) : crazy2 =
 match (c1,c2) with
 | (NIL,_) -> NIL
 | (_,NIL) ->NIL
 | (ONE(x), ONE(y)) -> ONE(c2carry(x,y))
 | (MONE(x), MONE(y)) -> MONE(c2carry(x,y))
 | (_,_) -> 
  ZERO(c2carry(inside(c1),inside(c2)))

let rec c2ex ((c1: crazy2), (c2:crazy2)) : crazy2 =
match(c1,c2) with
| (NIL,NIL) -> NIL
| (NIL, _) -> c2
| (_,NIL) -> c1
| (ZERO(x), MONE(y)) | (MONE(x), ZERO(y)) -> MONE(c2ex(x,y))
| (ZERO(x), ONE(y)) | (ONE(x), ZERO(y)) -> ONE(c2ex(x,y))
| (_,_)  ->
 ZERO(c2ex(inside(c1),inside(c2)))

let rec crazy2add ((c1: crazy2), (c2:crazy2)) : crazy2 = 
 let carry = c2carry(c1,c2) in
 let ex = c2ex(c1,c2) in
 if(crazy2val(carry) == 0) then ex else crazy2add(ZERO(carry), ex)
(*end*)
