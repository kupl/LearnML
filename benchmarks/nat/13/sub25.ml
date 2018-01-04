type nat = ZERO | SUCC of nat

let rec natval n = match n with
		 | ZERO -> 0
		 | SUCC x -> 1 + natval x

let rec natconv n = match n with
		  | 0 -> ZERO
		  | n -> SUCC (natconv (n - 1))

let natadd n = match n with
	     | (n0, n1) -> natconv(natval(n0) + natval(n1))

let natmul n = match n with
	     | (n0, n1) -> natconv(natval(n0) * natval(n1))
