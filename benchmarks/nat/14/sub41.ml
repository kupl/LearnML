type nat = 
  | ZERO
  | SUCC of nat
  (* (SUCC ZERO) = 1*)
let rec natadd : nat * nat -> nat = 
  fun (l, r) ->
    match l, r with
    | _, ZERO -> l
    | ZERO, _ -> r
    | _, SUCC r_in -> natadd ((SUCC l), r_in)
let rec natmul : nat * nat -> nat = 
  fun (l, r) ->
    match l, r with
    | _, ZERO -> ZERO
    | ZERO, _ -> ZERO
    | _ , (SUCC ZERO) -> l
    | (SUCC ZERO), _ -> r
    | _ as left, (SUCC r_in) -> natadd (left, natmul (left, r_in))
