type crazy2 =
  | NIL 
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let crazy2val x: int = 
  let rec crazy2evalrec a b: int = match a with
    | NIL -> 0
    | ZERO(y) -> crazy2evalrec y (2 * b)
    | ONE(y) -> b + crazy2evalrec y (2 * b)
    | MONE(y) -> -b + crazy2evalrec y (2 * b) in
  crazy2evalrec x 1
