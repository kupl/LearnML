(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
  fun n1 n2 -> match n1 with
  ZERO -> n2
  |_ ->
    (match n2 with
    ZERO -> n1
    | SUCC(tl) -> let v1 = SUCC(n1) in natadd v1 tl);;

let rec natmul : nat -> nat -> nat =
  fun n1 n2 -> match n1 with
  ZERO -> ZERO
  |_ ->
    (match n2 with
    ZERO -> ZERO
    | SUCC(tl) -> let sum = n1 in natadd sum (natmul n1 tl));;

let rec natexp : nat -> nat -> nat =
  fun n1 n2 -> match n1 with
  ZERO -> ZERO
  |_ ->
    (match n2 with
    ZERO -> SUCC ZERO
    | SUCC(tl) -> let pro = n1 in natmul pro (natexp n1 tl));;
