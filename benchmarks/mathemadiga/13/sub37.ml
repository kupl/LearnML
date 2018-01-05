type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp
and iter = NULL | FLOAT of float

exception FreeVariable

let galculator xp =
  let rec gal (xp, it) =
    match xp with
    | X ->
       (match it with
        | NULL -> raise FreeVariable
        | FLOAT f -> f)
    | INT n -> float_of_int n
    | REAL f -> f
    | ADD (xp1, xp2) -> gal (xp1, it) +. gal (xp2, it)
    | SUB (xp1, xp2) -> gal (xp1, it) -. gal (xp2, it)
    | MUL (xp1, xp2) -> gal (xp1, it) *. gal (xp2, it)
    | DIV (xp1, xp2) -> gal (xp1, it) /. gal (xp2, it)
    | SIGMA (xp1, xp2, xp3) ->
       (match (xp1, xp2) with
        | (INT n1, INT n2) ->
            if n1 > n2 then 0.
            else gal (xp3, FLOAT (float_of_int n1)) +.
                 gal (SIGMA (INT (n1+1), INT n2, xp3), NULL)
        | _ -> gal (SIGMA (INT (int_of_float (gal (xp1, it))),
                           INT (int_of_float (gal (xp2, it))),
                           xp3), NULL))
    | INTEGRAL (xp1, xp2, xp3) ->
       (match (xp1, xp2) with
        | (REAL f1, REAL f2) ->
            if f1 > f2 then -.(gal (INTEGRAL (xp2, xp1, xp3), NULL))
            else if f2 -. f1 < 0.1 then 0.
            else gal (xp3, FLOAT f1) *. 0.1 +.
                 gal (INTEGRAL (REAL (f1+.0.1), REAL f2, xp3), NULL)
        | _ -> gal (INTEGRAL (REAL (gal (xp1, it)),
                              REAL (gal (xp2, it)),
                              xp3), NULL)) in
  
  gal (xp, NULL)
