type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val: crazy2 -> int = fun crazy ->
  match crazy with 
  | NIL -> 0
  | ZERO a -> 2 * (crazy2val a)
  | ONE b -> 1 + 2 * (crazy2val b)
  | MONE c -> -1 + 2 * (crazy2val c)

let print_crazy crazy = print_int (crazy2val crazy); print_endline ""
