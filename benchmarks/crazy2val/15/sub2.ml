type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val number =
  match number with
  | NIL -> 0
  | ZERO number_ -> 2 * (crazy2val number_)
  | ONE number_ -> 2 * (crazy2val number_) + 1
  | MONE number_ -> 2 * (crazy2val number_) - 1
