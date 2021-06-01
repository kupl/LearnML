type nat = ZERO | SUCC of nat

let rec get_int
= fun n1 -> match n1 with
    | ZERO -> 0
    | SUCC (n2) -> 1 + get_int(n2);;
    
let rec get_nat
= fun a -> match a with
    | 0 -> ZERO
    | _ -> SUCC (get_nat (a-1));;
    
let natadd : nat -> nat -> nat
= fun n1 n2 -> get_nat ((get_int n1) + (get_int n2));;

let natmul : nat -> nat -> nat
= fun n1 n2 -> get_nat ((get_int n1) * (get_int n2));;
