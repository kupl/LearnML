type crazy2 = NIL
| ZERO of crazy2
| ONE of crazy2
| MONE of crazy2

let rec crazy2val(c2 : crazy2) : int =
match c2 with
|NIL -> 0
|ZERO c2_0 -> 2 * crazy2val(c2_0)
|ONE c2_1 -> 1 + 2 * crazy2val(c2_1)
|MONE c2_m1 -> -1 + 2 * crazy2val(c2_m1)

