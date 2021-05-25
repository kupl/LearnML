type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> let succ_n2 = SUCC (n2) in
    match n1 with
    ZERO     -> n2
  | SUCC (a) -> natadd a succ_n2;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let nat_match = (n1, n2) in
  match nat_match with
    (_, ZERO)
  | (ZERO, _)      -> ZERO
  | (SUCC (a) , b) -> natadd n2 (natmul a n2) ;;
