
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val c =
  let rec crazy2val2 c p r =
    match c with
      | NIL -> r
      | ZERO c_ -> crazy2val2 c_ (p + p) r
      | ONE c_ -> crazy2val2 c_ (p + p) (r + p)
      | MONE c_ -> crazy2val2 c_ (p + p) (r - p)
  in
    crazy2val2 c 1 0

