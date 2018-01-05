type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (first, second) -> 
 match second with
 |ZERO -> first
 |SUCC n -> SUCC (natadd (first, n))
 
let rec natmul : nat * nat -> nat = fun (first, second) -> 
 match second with
 |ZERO -> ZERO
 |SUCC n -> (natadd (natmul (first,n), first))
