(* C:\Users\saigoy\Desktop\nat.ml *)

type nat = ZERO
  | SUCC of nat;;

let rec natadd : nat * nat -> nat = fun (ln, rn) ->
  match ln with
  | ZERO -> rn
  | SUCC n -> (natadd (n, (SUCC rn)));;

let rec natmul : nat * nat -> nat = fun (ln , rn) ->
  match ln with
  | ZERO -> ZERO
  | SUCC n -> (natadd (rn, (natmul (n , rn) )) );;

