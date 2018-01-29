(* problem 2*)
type nat = ZERO | SUCC of nat

let rec print : int -> nat
= fun num ->
  if (num = 0) then ZERO
  else SUCC (print (num-1))

let rec expt b n =
  if (n = 0) then 1
  else b * (expt b (n-1))

let rec count : nat -> int -> int
= fun n1 num ->
  match n1 with
    | ZERO -> num
    | SUCC nat -> count nat num+1

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if(((count n1 0) = 0) && ((count n2 0) = 0)) then ZERO
  else print((count n1 0) + (count n2 0))

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if(((count n1 0) = 0) || ((count n2 0) = 0)) then ZERO
  else print((count n1 0) * (count n2 0))

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if((count n1 0) = 0) then ZERO
  else if((count n2 0) = 0) then SUCC ZERO
  else print((expt (count n1 0) (count n2 0)))