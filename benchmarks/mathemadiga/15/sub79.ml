type exp =
    |X
    |INT of int
    |REAL of float
    |ADD of exp * exp
    |SUB of exp * exp
    |MUL of exp * exp
    |DIV of exp * exp
    |SIGMA of exp * exp * exp
    |INTEGRAL of exp * exp * exp;;

exception FreeVariable;;

(*let rec sigma (a, b, f) =
    if a > b then
        0.0
    else if a = b then
        f (float_of_int a)
    else
        (f (float_of_int a)) +. sigma (a + 1, b, f);;*)

let sigma (a, b, f) =
  let rec aux a b f ret =
    if a > b then
      ret
    else
      aux (a + 1) b f (ret +. (f (float_of_int a)))
  in
  aux a b f 0.0;;

let integral (a, b, f) =
  let rec aux a b f ret =
    if a > b then
      0.0 -. (aux a b f ret)
    else if (a = b || b -. a < 0.1) then
      ret
    else
      aux (a +. 0.1) b f (ret +. (f a) *. 0.1)
    in
    aux a b f 0.0;;

(*let rec integral (a, b, f) =
    if a > b then
        0.0 -. integral (b, a ,f)
    else if (a = b || b -. a < 0.1) then
        0.0
    else
        (f a) *. 0.1 +. integral (a +. 0.1, b, f);;*)

let galculator exp =
    let rec aux exp flag x=
        match exp with
        |X -> if flag = 0 then raise FreeVariable else x
        |INT a -> (float_of_int a)
        |REAL a -> a
        |ADD (ex1, ex2) -> (aux ex1 flag x) +. (aux ex2 flag x)
        |SUB (ex1, ex2) -> (aux ex1 flag x) -. (aux ex2 flag x)
        |MUL (ex1, ex2) -> (aux ex1 flag x) *. (aux ex2 flag x)
        |DIV (ex1, ex2) -> (aux ex1 flag x) /. (aux ex2 flag x)
        |SIGMA (ex1, ex2, ex3) -> sigma (int_of_float (aux ex1 flag x), int_of_float (aux ex2 flag x), aux ex3 1)
        |INTEGRAL (ex1, ex2, ex3) -> integral (aux ex1 flag x, aux ex2 flag x, aux ex3 1)
    in
    aux exp 0 0.0;;

