(* 7 natadd : nat * nat -> nat
     natmul : nat * nat -> nat *)
type nat = ZERO | SUCC of nat

let rec natadd (nat0, nat1) = match (nat0, nat1) with
(ZERO, _) -> nat1
| (_, ZERO) -> nat0
| (n0, SUCC n1) -> natadd (SUCC n0, n1)

let natmul (nat0, nat1) =
let rec nattoint nat = match nat with
	ZERO -> 0
	| SUCC n -> 1 + (nattoint n) in
let rec inttonat i =
if i <= 0 then ZERO
	  else SUCC (inttonat (i-1)) in
inttonat ((nattoint nat0) * (nattoint nat1))
