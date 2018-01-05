(* hw2-4 *)

type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp
exception FreeVariable

let rec galculatorWithX x e =
  let rec sigmaTail sum a b c =
    if a > b then
      sum
    else
      sigmaTail (sum +. (galculatorWithX (Some (float_of_int a)) c)) (a + 1) b c
  in
  let rec integralTail sum a b c =
    if b -. a < 0.1 then
      sum
    else
      integralTail (sum +. 0.1 *. (galculatorWithX (Some a) c)) (a +. 0.1) b c
  in
  match e with
  | X -> (
    match x with
    | None -> raise FreeVariable
    | Some f -> f
  )
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (a, b) -> (galculatorWithX x a) +. (galculatorWithX x b)
  | SUB (a, b) -> (galculatorWithX x a) -. (galculatorWithX x b)
  | MUL (a, b) -> (galculatorWithX x a) *. (galculatorWithX x b)
  | DIV (a, b) -> (galculatorWithX x a) /. (galculatorWithX x b)
  | SIGMA (a, b, c) -> (
    let ga = int_of_float (galculatorWithX x a) in
    let gb = int_of_float (galculatorWithX x b) in
    sigmaTail 0. ga gb c
  )
  | INTEGRAL (a, b, c) -> (
    let ga = galculatorWithX x a in
    let gb = galculatorWithX x b in
    integralTail 0. ga gb c
  )

let galculator e = galculatorWithX None e
