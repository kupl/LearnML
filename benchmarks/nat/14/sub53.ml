(* 4190,310 Programming Language (Fall 2014)
 * Homework 1 - Exercise 3
 * CSE / 2012-13456 / Gao, Chengbin *)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
    match a with
    | ZERO -> b
    | SUCC(am) -> natadd (am, SUCC(b))

and natmul (a, b) =
    match a with
    | ZERO -> ZERO
    | SUCC(am) -> natadd (b, natmul (am, b)) (* a*b == b+(a-1)*b *)
