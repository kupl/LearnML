type nat = ZERO | SUCC of nat

let rec num: nat -> int
= fun n ->
    match n with
    ZERO -> 0
    | SUCC ZERO -> 1
    | SUCC(n') -> 1 + num n';;


let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let a = 0 in
    let rec foo : int -> nat -> nat
    = fun a n ->
      if a < (num n1) + (num n2) then foo (a+1) (SUCC(n))
      else n
    in foo a ZERO ;; 
    
let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  let a = 0 in
    let rec foo : int -> nat -> nat
    = fun a n ->
      if a < (num n1) * (num n2) then foo (a+1) (SUCC(n))
      else n
    in foo a ZERO ;; 
