type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
        match (n1, n2) with
        | (ZERO, ZERO) -> ZERO
        | (_, ZERO) -> n1
        | (ZERO, _) -> n2
        | (SUCC k, _) -> SUCC (natadd k n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
        match (n1, n2) with
        | (_, ZERO) -> ZERO
        | (ZERO, _) -> ZERO
        | (SUCC k, _) -> natadd n2 (natmul k n2);;
