(* 2015-11380 박찬양 HW1-4 *)

type nat = ZERO | SUCC of nat;;

let rec natadd: nat * nat -> nat = fun (n1, n2) ->
  match n1 with
  | ZERO -> n2
  | SUCC n -> SUCC (natadd(n, n2));;

let rec natmul: nat * nat -> nat = fun (n1, n2) ->
  match n1 with
  | ZERO -> ZERO
  | SUCC n -> natadd(n2, natmul(n, n2))

