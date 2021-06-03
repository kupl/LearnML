type nat =
	| ZERO
	| SUCC of nat;;

let rec tra_to_int : nat -> int
= fun n ->
match n with 
| ZERO -> 0
| SUCC ZERO -> 1
| SUCC (a) -> tra_to_int a + 1;;

let rec tra_to_nat : int -> nat
= fun n ->
match n with
| 0 -> ZERO
| 1 -> SUCC ZERO
| a -> SUCC (tra_to_nat (a-1));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
tra_to_nat (tra_to_int n1 + tra_to_int n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
tra_to_nat (tra_to_int n1 * tra_to_int n2);;
