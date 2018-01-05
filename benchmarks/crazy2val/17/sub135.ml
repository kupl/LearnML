type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val: crazy2 -> int = function x ->
match x with
| NIL -> 0
| ZERO x' -> 0 + 2 * (crazy2val x')
| ONE x' -> 1 + 2 * (crazy2val x')
| MONE x' -> -1 + 2 * (crazy2val x')
               
