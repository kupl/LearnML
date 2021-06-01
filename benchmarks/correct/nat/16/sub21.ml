type nat =
	| ZERO
	| SUCC of nat

let rec n_to_i x =
  match x with
  | ZERO -> 0
  | SUCC y -> 1 + (n_to_i y);;

let rec i_to_n x =
  match x with
  | 0 -> ZERO
  | _ -> SUCC (i_to_n (x-1));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> i_to_n ( (n_to_i n1) + (n_to_i n2) );;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> i_to_n ( (n_to_i n1) * (n_to_i n2) );;
