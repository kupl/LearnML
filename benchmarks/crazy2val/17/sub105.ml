exception Fatal
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (c : crazy2) : int =
  match c with
  | NIL     -> 0
  | ZERO tl -> (crazy2val tl) * 2
  | ONE  tl -> (crazy2val tl) * 2 + 1
  | MONE tl -> (crazy2val tl) * 2 - 1

