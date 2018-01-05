(*
  CSE/2015-21233/김종권
  Homework 2-5
*)
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

let rec galculator' e x =
  match e with
  | X ->
    (match x with
    | Some v ->
      v
    | None ->
      raise FreeVariable)
  | INT i -> float_of_int i
  | REAL r -> r
  | ADD (e1, e2) -> (galculator' e1 x) +. (galculator' e2 x)
  | SUB (e1, e2) -> (galculator' e1 x) -. (galculator' e2 x)
  | MUL (e1, e2) -> (galculator' e1 x) *. (galculator' e2 x)
  | DIV (e1, e2) -> (galculator' e1 x) /. (galculator' e2 x)
  | SIGMA (e1, e2, e3) ->
    let snum = int_of_float (galculator' e1 x) in
    let enum = int_of_float (galculator' e2 x) in
    let rec sigma a b acc =
      if a > b then acc
      else let result = (galculator' e3 (Some (float_of_int a))) in
        sigma (a+1) b (result +. acc)
    in
    sigma snum enum 0.0
  | INTEGRAL (e1, e2, e3) ->
    let snum = galculator' e1 x in
    let enum = galculator' e2 x in
    let rec integral a b acc =
      if (a +. 0.1) > b then acc else
      let result = (galculator' e3 (Some a)) *. 0.1 in
      integral (a +. 0.1) b (acc +. result)
    in
    if snum > enum then -. integral enum snum 0.0
    else integral snum enum 0.0
      
let rec galculator e = 
  galculator' e None
