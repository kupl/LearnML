
(* Exercise 4 *)
type nat = ZERO | SUCC of nat

(*
let rec nat_to_int : nat -> int = fun (n) ->
	match n with
	|ZERO -> 0
	|SUCC (nm) -> 1 + nat_to_int (nm) 
*)
let rec natadd : nat * nat -> nat = fun ( n1, n2 ) -> 
	match n2 with
	|ZERO -> n1
	|SUCC (n2m) ->
		match n1 with
		|ZERO -> n2
		|SUCC (n1m) -> natadd (SUCC (n1) , n2m)


let rec natmul : nat * nat -> nat = fun (n1, n2) ->
	match n2 with
	|ZERO -> ZERO
	|SUCC (n2m) ->
		match n1 with
		|ZERO -> ZERO
		|SUCC (n1m) -> natadd(n1, natmul(n1, n2m))
