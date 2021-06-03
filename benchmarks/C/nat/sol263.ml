
(*problem 2*)
type nat = ZERO | SUCC of nat
type oper = ADD | MUL | EXP

let rec p2_subfunc1 : nat-> int-> int = fun n1 int_1 ->
match n1 with 
	|ZERO -> int_1
	|SUCC (nat1)-> p2_subfunc1 nat1 (int_1+1)
let rec expo :int->int->int = fun n p ->
	if p = 0 then 1
	else if p =1 then n
	else n*(expo n (p-1))
let rec p2_subfunc2 : int -> int -> int-> oper -> nat -> nat = fun int_1 int_2 int_3 op nat1 ->
match op with 
	|ADD ->
		if int_3 = (int_1+int_2) then nat1
		else
		SUCC(p2_subfunc2 int_1 int_2 (int_3 + 1) ADD nat1)
	|MUL ->
		if int_3 = (int_1*int_2) then nat1
		else
		SUCC(p2_subfunc2 int_1 int_2 (int_3 + 1) MUL nat1)
	|EXP ->
		if int_3 = (expo int_1 int_2) then nat1
		else
		SUCC(p2_subfunc2 int_1 int_2 (int_3 + 1) EXP nat1)
let rec natadd : nat -> nat -> nat = fun n1 n2 ->
let a = p2_subfunc1 n1 0 in
	let b = p2_subfunc1 n2 0 in 
			p2_subfunc2 a b 0 ADD ZERO
let rec natmul : nat -> nat -> nat = fun n1 n2 ->
let a = p2_subfunc1 n1 0 in
	let b = p2_subfunc1 n2 0 in 
			p2_subfunc2 a b 0 MUL ZERO
let rec natexp : nat -> nat -> nat = fun n1 n2 ->
let a = p2_subfunc1 n1 0 in
	let b = p2_subfunc1 n2 0 in 
			p2_subfunc2 a b 0 EXP ZERO
;;