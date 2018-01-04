(*Computer Engineering 2015-12683 Kim Jaein Exercise 1-4*)
type nat = ZERO | SUCC of nat

let rec natadd ((a:nat), (b:nat)) =
	match a with
	|ZERO -> b
	|SUCC nat ->
		(match b with
		|ZERO -> a
		|SUCC nat -> natadd (SUCC a, nat))

let rec natmul ((a:nat), (b:nat)) =
	match a with
	|ZERO -> ZERO
	|SUCC nat ->
		(match b with
		|ZERO -> ZERO
		|SUCC nat -> natadd(a, (natmul (a, nat))))
