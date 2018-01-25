type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
  let rec size n =
    match n with
    | ZERO -> 0
    | SUCC(a) -> 1 + size(a)
  in let rec make n = 
    match n with
    | 0 -> ZERO
    | _ -> SUCC(make (n-1))
  in make (size n1 + size n2)
let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  let rec size n =
    match n with
    | ZERO -> 0
    | SUCC(a) -> 1 + size(a)
  in let rec make n = 
    match n with
    | 0 -> ZERO
    | _ -> SUCC(make (n-1))
  in make (size n1 * size n2)

