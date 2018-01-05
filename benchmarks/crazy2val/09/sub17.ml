type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val n=
match n with
NIL -> 0|
ZERO k -> 2*crazy2val k|
ONE k -> 1+2*crazy2val k|
MONE k -> -1+2*crazy2val k