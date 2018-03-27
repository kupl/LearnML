(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC(r) -> natadd r (SUCC(n2))
;;

let natmul : nat -> nat -> nat 
= fun n1 n2 ->
  let rec iter
  = fun c res ->
    match c with
    | ZERO -> res
    | SUCC(r) -> iter r (natadd n2 res)
  in
  iter n1 ZERO
;;

let natexp : nat -> nat -> nat 
= fun n1 n2 ->
  let rec iter
  = fun c res ->
    match c with
    | ZERO -> res
    | SUCC(r) -> iter r (natmul n1 res)
  in
  iter n2 (SUCC(ZERO))
;;