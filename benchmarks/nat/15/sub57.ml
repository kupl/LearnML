type nat = ZERO | SUCC of nat

let rec natadd n1 n2 = match n1 with
	|ZERO -> n2
	|SUCC k -> natadd k (SUCC n2)

let rec natmul n1 n2 = match n1 with
	|ZERO -> ZERO
	|SUCC k -> natadd n2 (natmul k n2)

(*
let four = SUCC (SUCC (SUCC (SUCC (ZERO))))
let three = SUCC (SUCC (SUCC (ZERO)))
let multi = natmul four three
let rec valnat n = match n with 
	|ZERO -> 0
	|SUCC k -> (valnat k) + 1
let _ = print_endline (string_of_int (valnat multi))
*)
