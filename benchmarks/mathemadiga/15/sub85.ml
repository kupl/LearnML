(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 5 *)

exception FreeVariable

(* type *)
type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

(* helper rec func *)
let rec eval x expression =
    let rec sigma start arrival expr value =
        if start > arrival then value
        else sigma (start + 1) arrival expr (value +. eval (Some (float_of_int start)) expr) in
    let rec integral start arrival expr value =
        if (arrival -. start) < 0.1 then value
        else integral (start +. 0.1) arrival expr (value +. (eval (Some start) expr) *. 0.1) in
    match expression with
    | X -> (match x with
            | Some value -> value
            | None -> raise FreeVariable)
    | INT value -> float_of_int value
    | REAL value -> value
    | ADD (e1, e2) -> (eval x e1) +. (eval x e2)
    | SUB (e1, e2) -> (eval x e1) -. (eval x e2)
    | MUL (e1, e2) -> (eval x e1) *. (eval x e2)
    | DIV (e1, e2) -> (eval x e1) /. (eval x e2)
    | SIGMA (start, arrival, expr) ->
            sigma (int_of_float (eval x start)) (int_of_float (eval x arrival)) expr 0.0
    | INTEGRAL (start, arrival, expr) ->
            let startEval = eval x start in
            let arrivalEval = eval x arrival in
            if startEval > arrivalEval then -. integral arrivalEval startEval expr 0.0
            else integral startEval arrivalEval expr 0.0

(* galculator *)
let galculator expr = eval None expr
