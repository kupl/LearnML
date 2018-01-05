type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception FreeVariable

let galculator e =
  let rec sigma lower upper f =
    if lower > upper then 0.0
    else f (float lower) +. sigma (lower + 1) upper f
  in
  let rec integral lower upper f =
    if lower > upper then -. integral upper lower f
    else if lower +. 0.1 > upper then 0.0
    else f lower *. 0.1 +. integral (lower +. 0.1) upper f
  in
  let rec doGalc e v =
    match e with
      X -> (match v with
        Some value -> value
      |  None -> raise FreeVariable)
    | INT i -> float i
    | REAL f -> f
    | ADD (subExp1, subExp2) -> doGalc subExp1 v +. doGalc subExp2 v
    | SUB (subExp1, subExp2) -> doGalc subExp1 v -. doGalc subExp2 v
    | MUL (subExp1, subExp2) -> doGalc subExp1 v *. doGalc subExp2 v
    | DIV (subExp1, subExp2) -> doGalc subExp1 v /. doGalc subExp2 v
    | SIGMA (subExp1, subExp2, subExp3) ->
      let lower = int_of_float @@ doGalc subExp1 v in
      let upper = int_of_float @@ doGalc subExp2 v in
      sigma lower upper @@ (fun x -> doGalc subExp3 @@ Some x)
    | INTEGRAL (subExp1, subExp2, subExp3) ->
      let lower = doGalc subExp1 v in
      let upper = doGalc subExp2 v in
      integral lower upper @@ (fun x -> doGalc subExp3 @@ Some x)
  in
  doGalc e None
