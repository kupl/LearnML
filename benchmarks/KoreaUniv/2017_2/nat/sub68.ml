(* problem 2 *)
type nat = ZERO | SUCC of nat
let rec natadd : nat -> nat -> nat = fun n1 n2 ->
     match n1 with
        ZERO -> n2
       |SUCC(k) -> SUCC(natadd k n2);;

let rec natmul : nat -> nat -> nat = fun n1 n2 ->
     match n1 with
        ZERO -> ZERO
       |SUCC(k) -> natadd (natmul k n2) n2;;

let rec natexp : nat -> nat -> nat = fun n1 n2 ->
     match n2 with
        ZERO -> SUCC(ZERO)
       |SUCC(k) -> natmul n1 (natexp n1 k);; 
