(* problem 2*) 
type nat = ZERO | SUCC of nat 

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC(k) -> natadd k (SUCC (n2))

let natmul : nat -> nat -> nat 
= fun n1 n2 -> if (n1 = ZERO) || (n2 = ZERO) then ZERO 
else
let rec multi : nat -> nat -> nat -> nat
= fun n1 n2 n3 -> match n1 with
| SUCC ZERO -> n2
| SUCC(k) -> multi k (natadd n3 n2) n3 in multi n1 n2 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec exp : nat -> nat -> nat -> nat
= fun n1 n2 n3 -> match n2 with
| ZERO -> SUCC ZERO
| SUCC(k) -> natmul (exp n1 k n3) n1 in
exp n1 n2 n1

