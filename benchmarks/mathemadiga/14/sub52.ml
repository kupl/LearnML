(* hw 2-4 *)
(* 2012-11269 DongJae Lim *)

exception FreeVariable

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

let rec calcSigma ((s : float), (e : float), (ex : exp)) : float =
  if s > e
  then 0.0
  else (calc (ex, REAL(s))) +. (calcSigma (s +. 1.0, e, ex))

and calcIntegral ((s : float), (e : float), (ex : exp)) : float =
  if e -. s < 0.1
  then 0.0
  else (calc (ex, REAL(s))) *. 0.1 +. (calcIntegral (s +. 0.1, e, ex))

and calc ((e : exp), (xv : exp)) : float =
  match e with
  | X ->
    (match xv with
     | REAL (x) -> x
     | _ -> raise FreeVariable)
  | INT (i) -> (float_of_int i)
  | REAL (f) -> f
  | ADD (e0, e1) -> (calc (e0, xv)) +. (calc (e1, xv))
  | SUB (e0, e1) -> (calc (e0, xv)) -. (calc (e1, xv))
  | MUL (e0, e1) -> (calc (e0, xv)) *. (calc (e1, xv))
  | DIV (e0, e1) -> (calc (e0, xv)) /. (calc (e1, xv))
  | SIGMA (es, ee, e) ->
    let vs = calc (es, xv) in
    let ve = calc (ee, xv) in
    calcSigma (vs, ve, e)
  | INTEGRAL (es, ee, e) ->
    let vs = calc (es, xv) in
    let ve = calc (ee, xv) in
    if vs > ve
    then calcIntegral (ve, vs, e) *. -1.0
    else calcIntegral (vs, ve, e)

let galculator (e : exp) : float =
  calc (e, X)
