exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val c =
  let rec crazy2val_sub c =
    match c with
      NIL -> 0
    | ZERO n -> 2 * (crazy2val_sub n)
    | ONE n -> 1 + 2 * (crazy2val_sub n)
    | MONE n -> -1 + 2 * (crazy2val_sub n)
  in
    if (c = NIL) then raise (Error "Exception")
	else (crazy2val_sub c)
