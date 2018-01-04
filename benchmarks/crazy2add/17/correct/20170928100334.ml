
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val: crazy2 -> int = function x ->
match x with
| NIL -> 0
| ZERO x' -> 0 + 2 * (crazy2val x')
| ONE x' -> 1 + 2 * (crazy2val x')
| MONE x' -> -1 + 2 * (crazy2val x')

let rec int2crazy: int -> crazy2 = function x ->
match x with
| 0 -> NIL
| _ ->
  let r = x mod 2 in
  let q = x / 2 in
  if r = 0 then ZERO (int2crazy q)
  else if r = 1 then ONE (int2crazy q)
  else MONE (int2crazy q)
      
let rec crazy2add: crazy2 * crazy2 -> crazy2 = function (x, y) ->
  let x' = crazy2val x in
  let y' = crazy2val y in
  int2crazy (x' + y')
