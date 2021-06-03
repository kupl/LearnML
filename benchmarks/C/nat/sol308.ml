type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
    match n1, n2 with
      ZERO, ZERO -> ZERO
    | SUCC x, ZERO -> SUCC x
    | ZERO, SUCC y -> SUCC y
    | SUCC x, SUCC y -> SUCC (natadd x (SUCC y));;

let rec 
natmul : nat -> nat -> nat
= fun n1 n2 ->
    match n1, n2 with
      ZERO, _ -> ZERO
    | _, ZERO -> ZERO
    | SUCC x, SUCC y -> natadd (SUCC x) (natmul (SUCC x) y);;

natadd (SUCC (SUCC (SUCC ZERO))) (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))));;
natmul (SUCC (SUCC (ZERO))) (SUCC (SUCC (SUCC (SUCC (ZERO)))));;
