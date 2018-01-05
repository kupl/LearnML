(* HW 1-6 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
	match x with ZERO -> ( match y with ZERO -> ZERO
					  | SUCC n -> y )
		   | SUCC m -> ( match y with ZERO -> x
				  	  | SUCC n -> SUCC (natadd (m, y)) )

let rec natmul (x, y) =
	let rec inner (n, sum, y) = 
		match n with ZERO -> sum
			   | SUCC n2 -> inner (n2, natadd (sum, y), y)
	in

	match x with ZERO -> ZERO
		   | SUCC m -> ( match y with ZERO -> ZERO
				  	  | SUCC n -> inner (m, y, y) )
					