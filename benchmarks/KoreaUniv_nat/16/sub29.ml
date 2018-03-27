type nat =
    | ZERO
    | SUCC of nat

let rec natval n = 
    match n with
    | ZERO -> 0
    | SUCC s -> 1 + natval s;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1, n2 with
    | ZERO, _ -> n2
    | _, ZERO -> n1
    | SUCC v1, SUCC v2 -> natadd v1 (SUCC n2);;

let rec natmul_helper n1 n2 result =
    match n1, n2 with 
        | _, ZERO -> result
        | ZERO, _ -> result
        | SUCC v1, _ -> natmul_helper v1 n2 (natadd result n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> natmul_helper n1 n2 ZERO;;
