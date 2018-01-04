exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (a, b) =
  let rec crazy2min c =
    let crazy2min_sub c =
      let n = (crazy2min c) in
     if n = ZERO NIL then NIL else n
    in
   match c with
     NIL -> NIL
   | ZERO NIL -> ZERO NIL
   | ZERO c -> ZERO (crazy2min_sub c)
   | ONE c -> ONE (crazy2min_sub c)
   | MONE c-> MONE (crazy2min_sub c)
  in
  let rec crazy2add_sub (a, b) =
    match (a, b) with
      (NIL, c) | (c, NIL) -> c
    | (ZERO c1, ZERO c2) | (ONE c1, MONE c2) | (MONE c1, ONE c2) -> ZERO (crazy2add_sub (c1, c2))
    | (ZERO c1, ONE c2) | (ONE c1, ZERO c2) -> ONE (crazy2add_sub (c1, c2))
	| (ZERO c1, MONE c2) | (MONE c1, ZERO c2) -> MONE (crazy2add_sub (c1, c2))
    | (ONE c1, ONE c2) -> ZERO (crazy2add_sub ((ONE NIL), (crazy2add_sub (c1, c2))))
	| (MONE c1, MONE c2) -> ZERO (crazy2add_sub ((MONE NIL), (crazy2add_sub (c1, c2))))
  in
    match (a, b) with
	  (NIL, c) | (c, NIL) -> raise (Error "Exception")
	| _ -> crazy2min (crazy2add_sub (a, b))
