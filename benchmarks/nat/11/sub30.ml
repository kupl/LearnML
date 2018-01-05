(* 컴퓨터공학부 2009-11833 창배성 *)
type nat = ZERO
	| SUCC of nat

let rec natadd (a, b) =
	match a with
	ZERO -> b
	| SUCC x -> natadd (x, SUCC b)

let rec intermul (i, j, k) =
	match i with
	SUCC ZERO -> j
	| SUCC SUCC x -> intermul (SUCC x, natadd (j, k), k)
	| _ -> raise ("Maybe some mistakes")

let natmul (a, b) =
	match a with
	ZERO -> ZERO
	| SUCC ZERO -> b
	| SUCC SUCC x -> match b with
		ZERO -> ZERO
		| SUCC ZERO -> a
		| SUCC SUCC x -> intermul (a, b, b)
		| _ -> raise ("Impossible?")
	| _ -> raise ("Invalid input")