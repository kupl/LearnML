(* problem 2*)
type nat = ZERO | SUCC of nat
let rec length n = 
  match n with
  ZERO -> 0
  |SUCC(x) -> 1+ (length x)

let rec mksuc n =
  match n with
  0-> ZERO
  |x-> SUCC(mksuc (x-1))


let natadd : nat -> nat -> nat 
= fun n1 n2 ->
  let sum = (length n1) + (length n2) in
    mksuc sum;;


let natmul : nat -> nat -> nat 
= fun n1 n2 -> 
  let mul = (length n1) * (length n2) in
    mksuc mul;;

