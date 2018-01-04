(* 2013-10894 지구환경과학부 강혁진 1-5번 문제*)

type nat = ZERO | SUCC of nat

(*let is_zero n =
	match n with
	| ZERO -> true
	| SUCC _ -> false
*)

let rec natadd (nadd1, nadd2)=
	match nadd1 with
	| ZERO -> nadd2
	| SUCC nadd1' -> SUCC (natadd (nadd1', nadd2))

let rec natmul (nmul1, nmul2)=
	match nmul1 with
	| ZERO -> ZERO
	| SUCC ZERO -> nmul2
	| SUCC (SUCC nmul1') ->
		match nmul2 with
		| ZERO -> ZERO
		| SUCC ZERO -> SUCC ZERO
		| SUCC (SUCC nmul2') ->
			match nmul2' with
			| ZERO -> natadd (nmul1, nmul1)
			| _ -> natadd (nmul1, (natmul (nmul1, (SUCC nmul2'))))
(*
let nat2 = SUCC(SUCC ZERO)
let nat3 = SUCC(SUCC(SUCC ZERO))
let nat0 = ZERO
let nat1 = SUCC ZERO
let nat4 = SUCC(SUCC(SUCC(SUCC ZERO)))
let nat5 = SUCC(SUCC(SUCC(SUCC(SUCC ZERO))))
*)
