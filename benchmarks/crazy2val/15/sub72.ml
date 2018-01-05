type crazy2 =
  |NIL
  |ZERO of crazy2
  |ONE of crazy2
  |MONE of crazy2

(*let crazy2val x = 
  let rec aux x ret mul=
    match x with
    |NIL -> ret
    |ZERO a -> aux a ret (mul*2)
    |ONE a -> aux a (ret + 1 * mul) (mul * 2)
    |MONE a -> aux a (ret - 1 * mul) (mul * 2)
  in
  aux x 0 1;;*)

let rec crazy2val x =
  match x with
  |NIL -> 0
  |ZERO a -> (2 * (crazy2val a))
  |ONE a -> 1 + 2 * (crazy2val a)
  |MONE a -> -1 + 2 * (crazy2val a)
