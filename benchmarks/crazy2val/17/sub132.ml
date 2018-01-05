type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun x -> (
  let rec crazy2val_temp x r s = match x with
  | NIL -> s
  | ZERO x' -> crazy2val_temp x' (r * 2) s
  | ONE x' -> crazy2val_temp x' (r * 2) (s + r)
  | MONE x' -> crazy2val_temp x' (r * 2) (s - r)
  in (crazy2val_temp x 1 0)
)
