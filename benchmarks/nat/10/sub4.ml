(* CSE/ 2004-11920 / Yeseong Kim/ Prob 6*)

type nat = ZERO | SUCC of nat

let rec natadd(n1, n2) = 
		match n1 with
			ZERO -> n2
		|	SUCC(n) -> SUCC(natadd(n,n2))

let rec natmul(n1, n2) =
		match n1 with
                        ZERO -> ZERO
                |	SUCC(n) -> natadd(natmul(n, n2), n2)
