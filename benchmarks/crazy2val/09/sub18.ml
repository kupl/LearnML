type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

exception Error of string

let crazy2val n=

let rec crazy2val_internal n=
match n with
NIL -> 0|
ZERO k -> 2*crazy2val_internal k|
ONE k -> 1+2*crazy2val_internal k|
MONE k -> -1+2*crazy2val_internal k
in

if n=NIL then
raise (Error "NIL is not number")
else
crazy2val_internal n