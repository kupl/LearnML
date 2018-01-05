(* Homework 1 - Exercise 4
 * 2011-10492 Jaeyeong Yang *)
type nat = ZERO
         | SUCC of nat

let rec natadd: nat * nat -> nat = fun (x, y) ->
  match x with
  | ZERO -> y
  | SUCC (xs) -> natadd (xs, SUCC (y))

let rec natmul: nat * nat -> nat = fun (x, y) ->
  match x with
  | ZERO -> ZERO
  | SUCC (ZERO) -> y
  | SUCC (xs) -> natadd (y, natmul (xs, y))

