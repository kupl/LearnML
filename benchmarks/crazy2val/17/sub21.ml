type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let crazy2val (c: crazy2) : int = 
  let rec op ((a: crazy2), (t: int)) = 
    match a with
    | NIL -> 0
    | ZERO a' -> op (a', (t * 2))
    | ONE a' -> (op (a', (t * 2))) + t
    | MONE a' -> (op (a', (t * 2))) - t
  in
  op (c, 1)