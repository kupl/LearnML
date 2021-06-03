(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
        match n2 with
        | ZERO -> n1
        | SUCC s -> natadd (SUCC n1) s

let rec natmul_rec n1 n2 r =
        match n2 with
        | ZERO -> r
        | SUCC s -> natmul_rec n1 s (natadd n1 r)
let natmul : nat -> nat -> nat 
= fun n1 n2 -> natmul_rec n1 n2 ZERO

let rec natexp_rec n1 n2 r =
        match n2 with
        | ZERO -> r
        | SUCC s -> natexp_rec n1 s (natmul n1 r)
let natexp : nat -> nat -> nat 
= fun n1 n2 -> natexp_rec n1 n2 (SUCC ZERO)