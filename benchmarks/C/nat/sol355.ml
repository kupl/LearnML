type nat = ZERO | SUCC of nat

let rec eval t =
    match t with
      | ZERO -> 0
      | SUCC (e) -> 1 + eval e;; 
  
let rec compose n =
  if n = 0 then ZERO
  else SUCC (compose (n - 1));;
  
let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1, n2 with
    | ZERO, ZERO -> ZERO
    | SUCC n, ZERO -> n1
    | ZERO, SUCC n -> n2
    | _ -> compose(eval n1 + eval n2);;
  

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
    match n1, n2 with
    | _, ZERO -> ZERO
    | ZERO, _ -> ZERO
    | _ -> compose(eval n1 * eval n2);;
