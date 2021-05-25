(*problem 2*)
type nat = ZERO|SUCC of nat;;

let rec  natadd : nat -> nat -> nat= fun n1 n2 ->
match n1 with
|ZERO-> n2
|SUCC m -> SUCC(natadd m n2);;

let rec natmul : nat -> nat-> nat = fun n1 n2 ->
match n2 with
|ZERO -> ZERO
|SUCC m -> natadd (natmul n1 m) (n1);;

let rec natexp : nat -> nat -> nat = fun n1 n2->
match n2 with
|ZERO ->SUCC(ZERO)
|SUCC m-> natmul(natexp n1 m) (n1);;
