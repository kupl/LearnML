type crazy2 = NIL | ONE of crazy2 | ZERO of crazy2 | MONE of crazy2

let rec crazy2val a =
match a with
|NIL->0
|ONE(c)->1+2*(crazy2val c)
|ZERO(c)->2*(crazy2val c)
|MONE(c)->(-1)+2*(crazy2val c)
;;




 
