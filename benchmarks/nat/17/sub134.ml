(* 2015 - 14718 Giyeon Kim HW 1 *)

(* Exercise 4 *)
type nat = ZERO | SUCC of nat

let rec natadd (left, right) =
    match right with
	| ZERO -> left
    | SUCC r -> natadd (SUCC left, r)

let rec natmul (left, right) =
    match right with
    | ZERO -> ZERO
    | SUCC r -> natadd (left, (natmul (left, r)))

