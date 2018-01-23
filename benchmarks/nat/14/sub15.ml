(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 1 Exercise 3  *)
type nat = ZERO | SUCC of nat

let rec natadd ((a:nat), (b:nat)) : nat =
  match (a, b) with
      (ZERO, _) -> b
    | (SUCC pre, _) -> natadd (pre, SUCC b)

let rec natmul ((a:nat), (b:nat)): nat =
  match (a, b) with
      (ZERO, _) -> ZERO
    | (SUCC pre, _) -> natadd (b, natmul (pre, b))
