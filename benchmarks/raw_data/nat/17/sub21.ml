(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
    match n1 with
    | ZERO -> n2
    | SUCC a -> natadd a (SUCC n2);;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> 
    let rec sub_natmul: nat -> nat -> nat -> nat
    = fun n1 n2 n ->
        match n1 with
        | ZERO -> ZERO
        | SUCC ZERO -> n2
        | SUCC a -> sub_natmul a (natadd n2 n) n
    in sub_natmul n1 n2 n2;;

let natexp : nat -> nat -> nat 
= fun n1 n2 ->
    let rec sub_natexp: nat -> nat -> nat -> nat
    = fun n n1 n2 ->
        match n2 with
        | ZERO -> if n1 = ZERO then raise(Failure "Error") else SUCC ZERO
        | SUCC ZERO -> n1
        | SUCC a -> sub_natexp n (natmul n n1) a
    in sub_natexp n1 n1 n2;;
