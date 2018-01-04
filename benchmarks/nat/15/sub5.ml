(*
    PL 1-5
    2008-11609 박성원
*)

type nat = ZERO | SUCC of nat
;;

let rec natadd (n, m) =
  match n with
  | ZERO -> m
  | SUCC k -> SUCC (natadd (k, m))
;;

let rec natmul (n, m) =
  match n with
  | ZERO -> ZERO
  | SUCC ZERO -> m
  | SUCC k -> natadd(m, natmul(k, m))
;;
