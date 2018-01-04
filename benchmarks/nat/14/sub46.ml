(* hw 1-3 *)
(* 2012-11269 DongJae Lim *)

type nat = ZERO | SUCC of nat

let rec natadd ((n1 : nat), (n2 : nat)) : nat =
  match n1 with
  | ZERO -> n2
  | SUCC (n) -> natadd (n, (SUCC n2))

let rec natmul ((n1 : nat), (n2 : nat)) : nat =
  match n1 with
  | ZERO -> ZERO
  | SUCC (n) -> natadd (n2, natmul (n, n2))
