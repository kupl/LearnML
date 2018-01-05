(*2009-11718 1-6*)
type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
	match (nat1, nat2) with
	(ZERO, ZERO) -> ZERO
	| (ZERO, SUCC a) -> SUCC (natadd (ZERO, a))
	| (SUCC a, ZERO) -> SUCC (natadd (a, ZERO))
	| (SUCC a, SUCC b) -> SUCC (SUCC (natadd (a,b)))

let rec natmul (nat1, nat2) =
	match (nat1, nat2) with
	(ZERO, ZERO) -> ZERO
	| (ZERO, SUCC a) -> ZERO
	| (SUCC a, ZERO) -> ZERO
	| (SUCC a, SUCC b) -> (match a with
			ZERO -> (natadd (ZERO, SUCC b))
			| SUCC c -> (natadd (SUCC b, (natmul (a, SUCC b)))))
		
(* 다른숫자들을 어케 표현하지 ㅡㅡ *)
