type nat = ZERO | SUCC of nat

let zero = ZERO;;
let one = SUCC ZERO;;
let two = SUCC(one);;
let three = SUCC(two);;
let four = SUCC(three);;
let five = SUCC(four);;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> n2
    | SUCC x -> SUCC (natadd x n2);;
(*x=n-1*)
let rec natmul: nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> ZERO
    | SUCC ZERO -> n2
    | SUCC x ->  natadd n2 (natmul x n2);;

natadd two three;;
natmul two three;;