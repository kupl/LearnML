type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with (*TODO*)
                ZERO -> n2
                | SUCC (a) -> natadd a (SUCC(n2));;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with (*TODO*)
                ZERO -> ZERO
                | SUCC (a) -> natadd n2 (natmul a n2);;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natmul two three;;
natadd two three;;
natadd ZERO two;;
natmul ZERO two;;