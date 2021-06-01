
(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
let rec impl m1 m2 =
  match m1 with
  | ZERO -> m2
  | SUCC (x) -> impl x (SUCC (m2)) in
impl n1 n2;;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
let rec impl m1 m2 =
  match m1 with
  | ZERO -> ZERO
  | SUCC (x) -> natadd (impl x m2) m2 in
impl n1 n2;;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
let rec impl m1 m2 =
  match m2 with
  | ZERO -> SUCC ZERO
  | SUCC (x) -> natmul (impl m1 x) m1 in
impl n1 n2;;

