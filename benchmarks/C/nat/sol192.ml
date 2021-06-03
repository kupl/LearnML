
(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  let rec helper n1 n2 =
    match n1 with
    | ZERO -> n2
    | SUCC (x) -> helper x (SUCC (n2)) in
      helper n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  let rec helper n1 n2 =
    match n1 with
    | ZERO -> ZERO
    | SUCC (x) -> natadd n2 (helper x n2) in
      helper n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  let rec helper n1 n2 =
    match n2 with
    | ZERO -> SUCC ZERO
    | SUCC (x) -> natmul n1 (helper n1 x) in
      helper n1 n2

