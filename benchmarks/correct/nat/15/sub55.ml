type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
                | ZERO -> n2
                | SUCC (a) -> natadd a (SUCC (n2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->   let rec natmul_sub a b c =
                match a with
                | ZERO -> b
                | SUCC (a) -> natmul_sub a ( natadd b c ) c in
                natmul_sub n1 ZERO n2

